import Lean.Data.Json
import Lean.Data.Lsp.Basic
import Std.Sync.Mutex

/-! Tinymist's stdio LSP client for the editor's synthetic Typst document. -/

open Lean

namespace Blog.Typst.Ide

private structure Tinymist where
  child : IO.Process.Child {
    stdin := .piped
    stdout := .piped
    stderr := .null
  }
  initialized : Bool := false
  nextId : Nat := 1
  document : Option Lsp.DocumentUri := none
  hostVersion : Option Int := none

private def tinymistSettings : Json := json% {
  exportPdf: "never",
  systemFonts: false,
  fontPaths: [],
  typstExtraArgs: ["--ignore-system-fonts"]
}

private def tinymistCapabilities : Json := json% {
  general: { positionEncodings: ["utf-16"] },
  textDocument: {
    hover: {
      contentFormat: ["markdown", "plaintext"]
    },
    completion: {
      contextSupport: true,
      completionItem: {
        snippetSupport: true,
        insertReplaceSupport: true,
        documentationFormat: ["markdown", "plaintext"]
      }
    }
  }
}

private def Tinymist.start : IO Tinymist := do
  let child ← IO.Process.spawn {
    cmd := "tinymist"
    args := #["lsp"]
    stdin := .piped
    stdout := .piped
    stderr := .null
    env := #[("TYPST_FONT_PATHS", none)]
  }
  pure { child }

private def Tinymist.stop (server : Tinymist) : IO Unit := do
  if (← server.child.tryWait).isSome then return
  server.child.kill
  discard server.child.wait

private def readExact (handle : IO.FS.Handle) (length : Nat) : IO ByteArray := do
  if length.toUSize.toNat != length then
    throw <| IO.userError "tinymist: Content-Length exceeds the addressable byte count"
  let mut bytes := ByteArray.emptyWithCapacity length
  while bytes.size < length do
    let chunk ← handle.read (length - bytes.size).toUSize
    if chunk.isEmpty then
      throw <| IO.userError s!"tinymist closed stdout after {bytes.size} of {length} message bytes"
    bytes := bytes ++ chunk
  pure bytes

private def readMessage (handle : IO.FS.Handle) : IO Json := do
  let mut length : Option Nat := none
  repeat
    let line ← handle.getLine
    if line.isEmpty then
      throw <| IO.userError "tinymist closed stdout while reading an LSP header"
    if line == "\r\n" then break
    unless line.endsWith "\r\n" do
      throw <| IO.userError "tinymist returned an LSP header without a CRLF terminator"
    match line.splitOn ":" with
    | name :: value :: rest =>
      if name.toLower == "content-length" then
        if length.isSome then
          throw <| IO.userError "tinymist returned duplicate Content-Length headers"
        let some count := value.trimAscii.toString.toNat?
          | throw <| IO.userError s!"tinymist returned an invalid Content-Length: {value.trimAscii}"
        unless rest.isEmpty do
          throw <| IO.userError "tinymist returned an invalid Content-Length header"
        length := some count
    | _ => throw <| IO.userError s!"tinymist returned an invalid LSP header: {line.trimAscii}"
  let some count := length
    | throw <| IO.userError "tinymist returned an LSP message without Content-Length"
  let bytes ← readExact handle count
  let some body := String.fromUTF8? bytes
    | throw <| IO.userError "tinymist returned an LSP message containing invalid UTF-8"
  match Json.parse body with
  | .ok message => pure message
  | .error error => throw <| IO.userError s!"tinymist returned invalid JSON: {error}"

private def Tinymist.send (server : Tinymist) (message : Json) : IO Unit := do
  if let some status ← server.child.tryWait then
    throw <| IO.userError s!"tinymist exited with status {status}"
  let body := message.compress.toUTF8
  server.child.stdin.putStr s!"Content-Length: {body.size}\r\n\r\n"
  server.child.stdin.write body
  server.child.stdin.flush

private def Tinymist.notify (server : Tinymist) (method : String) (params : Json) : IO Unit :=
  server.send <| Json.mkObj [
    ("jsonrpc", toJson "2.0"), ("method", toJson method), ("params", params)
  ]

private def rpcError (code : Int) (message : String) : Json :=
  Json.mkObj [("code", toJson code), ("message", toJson message)]

private def Tinymist.handleRequest
    (server : Tinymist) (id : Json) (method : String) : IO Unit := do
  server.send <| Json.mkObj [
    ("jsonrpc", toJson "2.0"), ("id", id),
    ("error", rpcError (-32601) s!"Unsupported tinymist client method: {method}")
  ]

private partial def Tinymist.receive
    (server : Tinymist) (id : Nat) (method : String) : IO Json := do
  let message ← readMessage server.child.stdout
  unless message.getObjValD "jsonrpc" == toJson "2.0" do
    throw <| IO.userError "tinymist returned a message without JSON-RPC version 2.0"
  if let .ok methodValue := message.getObjVal? "method" then
    let methodName ← match methodValue.getStr? with
      | .ok name => pure name
      | .error error => throw <| IO.userError s!"tinymist returned an invalid method: {error}"
    if let .ok requestId := message.getObjVal? "id" then
      server.handleRequest requestId methodName
    -- Notifications, including diagnostics and progress, are not results of this query.
    server.receive id method
  else
    let responseId ← match message.getObjVal? "id" with
      | .ok value => pure value
      | .error error => throw <| IO.userError s!"tinymist returned a response without an id: {error}"
    unless responseId == toJson id do
      throw <| IO.userError s!"tinymist response id mismatch for {method}: expected {id}, got {responseId.compress}"
    if let .ok error := message.getObjVal? "error" then
      throw <| IO.userError s!"tinymist {method} failed: {error.compress}"
    match message.getObjVal? "result" with
    | .ok result => pure result
    | .error error => throw <| IO.userError s!"tinymist {method} returned no result: {error}"

private def Tinymist.request
    (server : Tinymist) (method : String) (params : Json) : IO (Json × Tinymist) := do
  let id := server.nextId
  server.send <| Json.mkObj [
    ("jsonrpc", toJson "2.0"), ("id", toJson id),
    ("method", toJson method), ("params", params)
  ]
  let result ← server.receive id method
  pure (result, { server with nextId := id + 1 })

private def Tinymist.initialize (server : Tinymist) : IO Tinymist := do
  if server.initialized then return server
  let pid ← IO.Process.getPID
  let params := Json.mkObj [
    ("processId", toJson pid.toNat),
    ("clientInfo", json% { name: "space embedded Typst" }),
    ("rootUri", Json.null),
    ("capabilities", tinymistCapabilities),
    ("initializationOptions", tinymistSettings)
  ]
  let (result, server) ← server.request "initialize" params
  let capabilities ← match result.getObjVal? "capabilities" with
    | .ok value => pure value
    | .error error => throw <| IO.userError s!"tinymist initialize returned no capabilities: {error}"
  let _ ← match capabilities.getObj? with
    | .ok value => pure value
    | .error error => throw <| IO.userError s!"tinymist initialize returned invalid capabilities: {error}"
  if let .ok encoding := capabilities.getObjVal? "positionEncoding" then
    unless encoding == toJson "utf-16" do
      throw <| IO.userError s!"tinymist selected unsupported position encoding: {encoding.compress}"
  server.notify "initialized" (Json.mkObj [])
  pure { server with initialized := true }

private def Tinymist.updateDocument
    (server : Tinymist) (uri : Lsp.DocumentUri) (hostVersion : Int)
    (source : String) : IO Tinymist := do
  if server.document == some uri && server.hostVersion == some hostVersion then return server
  if server.document == some uri then
    server.notify "textDocument/didChange" <| Json.mkObj [
      ("textDocument", Json.mkObj [("uri", toJson uri), ("version", toJson hostVersion)]),
      ("contentChanges", Json.arr #[Json.mkObj [("text", toJson source)]])
    ]
  else
    if let some previous := server.document then
      server.notify "textDocument/didClose" <| Json.mkObj [
        ("textDocument", Json.mkObj [("uri", toJson previous)])
      ]
    server.notify "textDocument/didOpen" <| Json.mkObj [
      ("textDocument", Json.mkObj [
        ("uri", toJson uri), ("languageId", toJson "typst"),
        ("version", toJson hostVersion), ("text", toJson source)
      ])
    ]
  pure { server with document := some uri, hostVersion := some hostVersion }

private def Tinymist.query
    (server : Tinymist) (uri : Lsp.DocumentUri) (hostVersion : Int) (source method : String)
    (params : Json) : IO (Json × Tinymist) := do
  let server ← server.initialize
  let server ← server.updateDocument uri hostVersion source
  server.request method params

private def queryTimeoutMs : Nat := 15000

private def checkQuery (cancelled : IO Bool) (deadline : Nat) : IO Unit := do
  if ← cancelled then
    throw <| IO.userError "tinymist request cancelled"
  if (← IO.monoMsNow) ≥ deadline then
    throw <| IO.userError s!"tinymist request timed out after {queryTimeoutMs} ms"

private partial def awaitQuery {α : Type}
    (task : Task (Except IO.Error α)) (cancelled : IO Bool) (deadline : Nat) : IO α := do
  checkQuery cancelled deadline
  if ← IO.hasFinished task then
    match ← IO.wait task with
    | .ok result => pure result
    | .error error => throw error
  else
    IO.sleep 10
    awaitQuery task cancelled deadline

-- Module initialization is per Lean worker process, independently of the renderer's mutex.
private initialize tinymistState : Std.Mutex (Option Tinymist) ← Std.Mutex.new none

private partial def queryBefore
    (uri : Lsp.DocumentUri) (hostVersion : Int) (source method : String) (params : Json)
    (cancelled : IO Bool) (deadline : Nat) : IO Json := do
  checkQuery cancelled deadline
  let result ← tinymistState.tryAtomically do
    checkQuery cancelled deadline
    let server ← match ← get with
      | some server => pure server
      | none =>
        try
          Tinymist.start
        catch error =>
          throw <| IO.userError s!"Could not start tinymist lsp: {error}"
    set (none : Option Tinymist)
    let task ← IO.asTask (prio := .dedicated) (server.query uri hostVersion source method params)
    try
      let (result, server) ← awaitQuery task cancelled deadline
      set (some server)
      pure result
    catch error =>
      try
        server.stop
      catch stopError =>
        throw <| IO.userError s!"{error}; could not terminate and reap tinymist: {stopError}"
      discard <| IO.wait task
      throw <| IO.userError s!"tinymist query {method}: {error}"
  match result with
  | some result => pure result
  | none =>
    IO.sleep 10
    queryBefore uri hostVersion source method params cancelled deadline

/-- Queries Tinymist, synchronizing the synthetic source once per Lean document version. -/
def query (uri : Lsp.DocumentUri) (version : Int) (source : String) (method : String)
    (params : Json) (cancelled : IO Bool) : IO Json := do
  queryBefore uri version source method params cancelled ((← IO.monoMsNow) + queryTimeoutMs)

end Blog.Typst.Ide
