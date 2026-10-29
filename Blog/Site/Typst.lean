import VersoBlog
import Verso.Doc.ArgParse
import Verso.Doc.Elab
import Verso.Parser
import Blog.Site.Typst.Config
import Blog.Site.Typst.Ide
import Blog.Site.Typst.Renderer
import Blog.Site.Typst.Runtime
import Blog.Site.Typst.Registry

/-! Verso Typst roles and fenced blocks, rendered together at the end of each Lean file. -/

open Lean
open Verso Genre Blog Doc Elab ArgParse Output Html

namespace Blog.Typst


/-- Chooses the Lean source span for a compiler or page-boundary diagnostic. -/
def Diagnostic.ref (diag : Diagnostic) (document : Ide.Document) (fallback : Syntax) : Syntax :=
  let exact? := do
    let start ← diag.start
    let stop ← diag.stop
    document.ref? start stop
  let boundary? := do
    let id ← diag.fragment
    let segment ← (document.segmentsById[id]?).join
    return Syntax.ofRange { start := segment.fragment.start, stop := segment.fragment.stop }
  (exact? <|> boundary?).getD fallback

open Lean.Elab.Command in
/-- Compiles the file's registered fragments and embeds their SVG table at EOF. -/
@[command_elab Lean.Parser.Command.eoi]
def finalizeTypst : CommandElab := fun stx => do
  let env ← getEnv
  let entries := sourceEntries env
  if entries.isEmpty then return
  let document := Ide.Document.create entries
  let fallback := (document.segments[0]?.map fun segment =>
    Syntax.ofRange { start := segment.fragment.start, stop := segment.fragment.stop }).getD stx
  let response ← try
      run document.source entries.size
    catch error =>
      throwErrorAt fallback error.toMessageData
  liftCoreM Core.checkInterrupted
  let mut hasErrors := false
  for diagnostic in response.diagnostics do
    let ref := diagnostic.ref document fallback
    if diagnostic.kind == "warning" then
      logWarningAt ref diagnostic.format
    else if diagnostic.kind == "error" then
      hasErrors := true
      logErrorAt ref diagnostic.format
  if hasErrors then return
  unless response.success do
    throwErrorAt fallback "Typst batch compilation failed without an error diagnostic"
  let file := fileKey env (← getFileName)
  install file response.outputs
  let outputs : Array (TSyntax `term) := response.outputs.map quote
  elabCommand (← `(initialize do
    Blog.Typst.install $(quote file) #[$outputs,*]))

end Blog.Typst

open Lean.Doc.Syntax

/-- Embeds a Typst code span, using inline math by default. -/
@[role]
def typst : RoleExpanderOf Blog.Typst.Config
  | config, contents => do
      let inl ← match contents with
        | #[inl] => pure inl
        | _ => throwError "Expected precisely one Typst code span, got {contents}"
      let str ← match inl with
        | `(inline| code( $s:str )) => pure s
        | _ => throwErrorAt inl "Expected a code span containing Typst source"
      let payload ← Blog.Typst.registerFragment config str
      ``(Verso.Doc.Inline.other
        (Blog.InlineExt.component $(quote ``Blog.Typst.renderedTypst)
          (toJson (Blog.Typst.Payload.mk $(quote payload.file) $(quote payload.id) $(quote payload.display))))
        #[])

/-- Embeds a rendered Typst block, using block-level markup by default. -/
@[code_block typst]
def typstBlock : CodeBlockExpanderOf Blog.Typst.BlockConfig
  | config, str => do
      let payload ← Blog.Typst.registerFragment config.toConfig str
      ``(Verso.Doc.Block.other
        (Blog.BlockExt.component $(quote ``Blog.Typst.renderedTypst)
          (toJson (Blog.Typst.Payload.mk $(quote payload.file) $(quote payload.id) $(quote payload.display))))
        #[])
