import Blog.Site.Typst.SourceMap
import Blog.Site.Typst.Runtime
import Verso.Doc.Elab

/-! File-local Typst source registration, carried by Lean's elaboration snapshots. -/

open Lean Verso.Doc.Elab

namespace Blog.Typst

-- PersistentArray shares prior command snapshots instead of copying a growing flat array.
private initialize sourceExt : EnvExtension (PersistentArray Ide.ScopedFragment) ←
  registerEnvExtension (pure {})

/-- Returns the current file's fragments in registration order. -/
def sourceEntries (env : Environment) : Array Ide.ScopedFragment :=
  (sourceExt.getState env).toArray

/-- Identifies an SVG batch by its compiled module and source file. -/
def fileKey (env : Environment) (fileName : String) : String :=
  s!"{env.mainModule}:{fileName}"

/-- Registers a source fragment and returns its component reference for site rendering. -/
def registerFragment (config : Config) (str : StrLit) : DocElabM Payload := do
  let fragment := Ide.fragmentOfSyntax (← getFileMap) str config
  let state ← readThe PartElabM.State
  let root := state.partContext.parents[0]?.getD state.partContext.toPartFrame
  let some document := state.documentPos? <|> root.selectionSyntax.getPos?
    | throwErrorAt str "Typst shared context requires a document source position"
  let env ← getEnv
  let id := (sourceExt.getState env).size
  modifyEnv fun env => sourceExt.modifyState env
    (·.push { id, document := document.byteIdx, fragment })
  return { file := fileKey env (← getFileName), id, display := config.display }

end Blog.Typst
