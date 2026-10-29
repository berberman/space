import Blog.Site.Typst.Protocol
import Std.Sync.Mutex

/-! Batch SVG rendering over a persistent `space-typst` subprocess. -/

open Lean

namespace Blog.Typst

private structure Process where
  child : IO.Process.Child {
    stdin := .piped
    stdout := .piped
    stderr := .inherit
  }
  nextId : Nat := 1

private def startProcess : IO Process := do
  let child ← IO.Process.spawn {
    cmd := "typst/target/release/space-typst"
    args := #[]
    stdin := .piped
    stdout := .piped
    stderr := .inherit
  }
  pure { child }

private def Process.killQuietly (process : Process) : IO Unit := do
  try process.child.kill catch _ => pure ()

private def Process.call (process : Process) (source : String) (fragments : Nat) :
    IO (Response × Process) := do
  if let some exitCode ← process.child.tryWait then
    throw <| IO.userError s!"space-typst has already exited with status {exitCode}"
  let id := process.nextId
  let request : Request := { id, source, fragments }
  process.child.stdin.putStrLn (toJson request).compress
  process.child.stdin.flush
  let line ← process.child.stdout.getLine
  if line.isEmpty then
    let status ← process.child.tryWait
    throw <| IO.userError <| match status with
      | some exitCode => s!"space-typst terminated with status {exitCode}"
      | none => "space-typst closed stdout unexpectedly"
  let payload := line.trimAscii.toString
  if payload.isEmpty then
    throw <| IO.userError "space-typst returned an empty response"
  let json ← match Json.parse payload with
    | .ok json => pure json
    | .error error => throw <| IO.userError s!"space-typst returned invalid JSON:\n{error}\n\nresponse:\n{payload}"
  let response : Response ← match FromJson.fromJson? json with
    | .ok response => pure response
    | .error error => throw <| IO.userError s!"space-typst returned an invalid response:\n{error}\n\nresponse:\n{payload}"
  if response.id != id then
    throw <| IO.userError s!"space-typst RPC desynchronized: expected response id {id}, got {response.id}"
  if response.success then
    unless response.outputs.size == fragments do
      throw <| IO.userError s!"space-typst returned {response.outputs.size} outputs for {fragments} fragments"
  else unless response.outputs.isEmpty do
    throw <| IO.userError "space-typst returned outputs for a failed compilation"
  pure (response, { process with nextId := id + 1 })

private initialize processState : Std.Mutex (Option Process) ← Std.Mutex.new none

/-- Compiles a complete synthetic file, returning SVGs in fragment-index order. -/
def run (source : String) (fragments : Nat) : IO Response :=
  processState.atomically do
    let process ← match ← get with
      | some process => pure process
      | none => startProcess
    try
      let (response, process') ← process.call source fragments
      set (some process')
      pure response
    catch error =>
      set (none : Option Process)
      process.killQuietly
      throw error

end Blog.Typst
