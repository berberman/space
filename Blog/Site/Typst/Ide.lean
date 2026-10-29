import Blog.Site.Typst.Client
import Blog.Site.Typst.SourceMap
import Lean.Server
import Lean.Language.Lean

/-! Typst hover and completion through Lean's language server, with one cached source map per file version. -/

open Lean Server Lsp

namespace Blog.Typst.Ide

private def checkCurrent : RequestM Unit := do
  let tk := (← read).cancelTk
  if ← tk.wasCancelledByEdit then throw .fileChanged
  if ← tk.wasCancelledByCancelRequest then throw .requestCancelled

private def forward (method : String) (params : Json) (document : Document)
    (segment : DocumentSegment) (position : String.Pos.Raw) : RequestM Json := do
  checkCurrent
  let doc ← RequestM.readDoc
  -- A sibling URI preserves the host's project root without writing a temporary file.
  let uri := doc.meta.uri ++ ".typst.typ"
  let fields ← EIO.ofExcept <| params.getObj?.mapError RequestError.invalidParams
  let params := Json.obj <| fields
    |>.insert "textDocument" (Json.mkObj [("uri", toJson uri)])
    |>.insert "position" (toJson <| document.virtualPosition segment position)
  let tk := (← read).cancelTk
  let result ← try
      query uri (Int.ofNat doc.meta.version) document.source method params tk.wasCancelled
    catch error =>
      checkCurrent
      throw error
  checkCurrent
  EIO.ofExcept <| (if method == "textDocument/hover" then
    document.mapHover segment doc.meta.text result
  else
    document.mapCompletion segment doc.meta.text doc.meta.uri result).mapError RequestError.internalError

private def collectCommands (initial : Language.Lean.InitialSnapshot) : BaseIO (Option (Environment × Array Syntax)) := do
  let some parsed := initial.result? | return none
  let header ← parsed.processedSnap.task.asServerTask.wait
  let some state := header.result? | return none
  let mut commands := #[]
  let mut snap ← state.firstCmdSnap.task.asServerTask.wait
  repeat
    commands := commands.push snap.stx
    match snap.nextCmdSnap? with
    | some next => snap ← next.task.asServerTask.wait
    | none => return some (state.cmdState.env, commands)

private structure CachedDocument where
  uri : DocumentUri
  version : Nat
  task : ServerTask Document

-- Replacing the entry releases old versions once their in-flight requests finish.
private initialize documentCache : Std.Mutex (Option CachedDocument) ← Std.Mutex.new none

private def fileDocumentTask (metadata : DocumentMeta) (initial : Language.Lean.InitialSnapshot)
    : BaseIO (ServerTask Document) :=
  documentCache.atomically do
    if let some cached ← get then
      if cached.uri == metadata.uri && cached.version == metadata.version then
        return cached.task
    let task ← ServerTask.BaseIO.asTask do
      match ← collectCommands initial with
      | some (env, commands) => return fileDocument env metadata.text commands
      | none => return Document.create #[]
    set <| some ({ uri := metadata.uri, version := metadata.version, task } : CachedDocument)
    return task


private def handle (method : String) (params : Json) (previous : RequestTask Json)
    : RequestM (RequestTask Json) := do
  checkCurrent
  let doc ← RequestM.readDoc
  let lspPosition ← EIO.ofExcept <|
    params.getObjValAs? Lsp.Position "position" |>.mapError RequestError.invalidParams
  let position := doc.meta.text.lspPosToUtf8Pos lspPosition
  let current := Language.Lean.findCmdParsedSnap doc.initSnap doc.meta.text position
  RequestM.bindTaskCheap current.asServerTask fun current => do
    checkCurrent
    let some current := current | return previous
    let some header := doc.initSnap.result?.bind (·.processedSnap.task.get.result?) | return previous
    if !containsTypst header.cmdState.env doc.meta.text current.stx position then return previous
    -- Parse-only snapshots remain usable while Typst elaboration fails.
    let document ← fileDocumentTask doc.meta doc.initSnap
    RequestM.bindTaskCheap document fun document => do
      checkCurrent
      let some segment := document.segmentAt? position | return previous
      RequestM.asTask <| forward method params document segment position

private def handleResolve (item : Json) (previous : RequestTask Json)
    : RequestM (RequestTask Json) := do
  if (item.getObjVal? "data" >>= (·.getObjValAs? Bool "typstIde")).toOption == some true then
    return .pure item
  return previous

initialize
  chainLspRequestHandler "textDocument/hover" Json Json (handle "textDocument/hover")
  chainLspRequestHandler "textDocument/completion" Json Json (handle "textDocument/completion")
  chainLspRequestHandler "completionItem/resolve" Json Json handleResolve

end Blog.Typst.Ide
