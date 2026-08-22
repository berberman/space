import VersoBlog
import Verso.Doc.ArgParse
import Verso.Doc.Elab
import Verso.Parser
import Lean.Data.Json
import Std.Sync.Mutex

open Lean

open Verso Genre Blog Doc Elab ArgParse Output Html

namespace Blog.Typst

structure Request where
  id : Nat
  source : String
  display : Bool
deriving ToJson

structure Diagnostic where
  kind : String
  message : String
  /--
  Byte offsets into `Request.source`.
  -/
  start : Option Nat
  stop : Option Nat
  hints : Array String
deriving FromJson, Repr, Inhabited

def Diagnostic.ref (diag : Diagnostic) (sourceStx : Syntax) : Syntax :=
  match diag.start, diag.stop,
        sourceStx.getPos?, sourceStx.getTailPos? with
  | some start, some stop, some sourceStart, some sourceStop =>
      let startPos := sourceStart.increaseBy start
      let stopPos := sourceStart.increaseBy stop
      if startPos.byteIdx ≤ stopPos.byteIdx &&
         stopPos.byteIdx ≤ sourceStop.byteIdx then
        Syntax.ofRange {
          start := startPos
          stop := stopPos
        }
      else sourceStx
  | _, _, _, _ => sourceStx

structure Response where
  id : Nat
  svg : Option String
  diagnostics : Array Diagnostic
deriving FromJson, Repr

def Diagnostic.isError (diag : Diagnostic) : Bool :=
  diag.kind == "error"

def Diagnostic.isWarning (diag : Diagnostic) : Bool :=
  diag.kind == "warning"

def Diagnostic.format (diag : Diagnostic) : String :=
  if diag.hints.isEmpty then
    s!"Typst: {diag.message}"
  else
    let hints :=
      diag.hints.toList.map fun hint =>
        s!"  hint: {hint}"

    s!"Typst: {diag.message}\n{String.intercalate "\n" hints}"

private structure Process where
  child : IO.Process.Child {
    stdin := .piped
    stdout := .piped
    stderr := .inherit
  }
  nextId : Nat := 1

private def startProcess : IO Process := do

  -- stdout is only for NDJSON
  let child ← IO.Process.spawn {
    cmd := "typst/target/release/space-typst"
    args := #[]
    stdin := .piped
    stdout := .piped
    stderr := .inherit
  }

  pure {
    child
    nextId := 1
  }


private def Process.killQuietly (process : Process) : IO Unit := do
  try
    process.child.kill
  catch _ =>
    pure ()


private def Process.call (process : Process) (source : String) (display : Bool) : IO (Response × Process) := do

  if let some exitCode ← process.child.tryWait then
    throw <| IO.userError s!"space-typst has already exited with status {exitCode}"

  let id := process.nextId

  let request : Request := {
    id
    source
    display
  }

  -- Make it one-line
  let requestJson := toJson request |>.compress

  process.child.stdin.putStrLn requestJson
  process.child.stdin.flush

  -- This blocks until exactly one response is available
  let line ← process.child.stdout.getLine

  if line.isEmpty then
    let status ← process.child.tryWait

    throw <| IO.userError <|
      match status with
      | some exitCode => s!"space-typst terminated with status {exitCode}"
      | none => "space-typst closed stdout unexpectedly"

  let payload := line.trimAscii.toString

  if payload.isEmpty then
    throw <| IO.userError "space-typst returned an empty response"

  let json ←
    match Json.parse payload with
    | .ok json => pure json
    | .error error => throw <| IO.userError s!"
space-typst returned invalid JSON:
{error}

response:
{payload}"

  let response : Response ←
    match FromJson.fromJson? (α := Response) json with
    | .ok response => pure response
    | .error error => throw <| IO.userError s!"
space-typst returned an invalid response:
{error}

response:
{payload}"

  if response.id != id then
    throw <| IO.userError s!"
space-typst RPC desynchronized:
expected response id {id}, got {response.id}"

  pure (
    response,
    {
      process with
      nextId := id + 1
    }
  )


/--
Mutex for one process per Lean process
-/
initialize processState : Std.Mutex (Option Process) ← Std.Mutex.new none

def run (source : String) (display : Bool) : IO Response :=
  processState.atomically do
    let process ←
      match ← get with
      | some process => pure process
      | none =>  startProcess

    try
      let (response, process') ← process.call source display
      set (some process')
      pure response
    catch error =>
      set (none : Option Process)
      process.killQuietly
      throw error

def restart : IO Unit :=
  processState.atomically do
    if let some process ← get then process.killQuietly
    set (none : Option Process)

structure Config where
  display : Bool

section

variable [Monad m] [MonadError m]

def Config.parse : ArgParse m Config :=
  Config.mk <$> .flag `display false

instance : FromArgs Config m :=
  ⟨Config.parse⟩

end

end Blog.Typst

open Lean.Doc.Syntax

@[role]
def typst : RoleExpanderOf Blog.Typst.Config
  | config, contents => do
      let inl ←
        match contents with
        | #[inl] => pure inl
        | _ => throwError "Expected precisely one Typst code span, got {contents}"
      let (srcStx, source) ←
        match inl with
        | `(inline| code( $s:str )) => pure (s.raw , s.getString)
        | _ => throwErrorAt inl "Expected a code span containing Typst source"
      let response ←
        try
          Blog.Typst.run source config.display
        catch error =>
          throwErrorAt inl error.toMessageData

      for diagnostic in response.diagnostics do
        if diagnostic.isWarning then
          logWarningAt (diagnostic.ref srcStx) diagnostic.format

      let errors := response.diagnostics.filter (·.isError)

      if h : errors.size > 0 then
        for diagnostic in errors[0 : errors.size - 1] do
          logErrorAt (diagnostic.ref srcStx) diagnostic.format

        let diagnostic := errors[errors.size - 1]
        throwErrorAt (diagnostic.ref srcStx) diagnostic.format

      let svg ←
        match response.svg with
        | some svg => pure svg
        | none =>  throwErrorAt inl "Typst compilation produced neither SVG nor an error"

      let html : Html :=
        if config.display then
          {{
            <span class="typst-block">
              {{ Html.text false svg }}
            </span>
          }}
        else
          {{
            <span class="typst-inline">
              {{ Html.text false svg }}
            </span>
          }}
      `(
        _root_.Verso.Doc.Inline.other
          (Blog.InlineExt.blob $(quote html))
          #[]
      )
