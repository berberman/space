import Blog.Site.LeanRecall.Range
import Blog.Site.LeanRecall.Loader
import Verso.Doc.ArgParse
import Verso.Doc.Elab
import Verso.Doc.Suggestion
import VersoBlog

open Lean Verso Doc Elab ArgParse
open SubVerso.Highlighting
open SubVerso.Module

namespace Blog.LeanRecall

structure Config where
  moduleName : Name
  declName : Name
  start : Option Nat := none
  stop : Option Nat := none
  startLine : Option Nat := none
  stopLine : Option Nat := none
  occurrence : Nat := 1
  project : String := "."
  showProofStates : Bool := true

section

variable [Monad m] [MonadError m]

def Config.parse : ArgParse m Config :=
  Config.mk <$>
    .positional `module .name <*>
    .positional `decl .name <*>
    .named `start .nat true <*>
    .named `stop .nat true <*>
    .named `startLine .nat true <*>
    .named `stopLine .nat true <*>
    .namedD `occurrence .nat 1 <*>
    .namedD `project .string "." <*>
    .flag `showProofStates true

instance : FromArgs Config m := ⟨Config.parse⟩

end

def resolveRange (source body : String) (cfg : Config) : Except String (Nat × Nat) := do
  match cfg.start, cfg.stop, cfg.startLine, cfg.stopLine with
  | some start, some stop, none, none => pure (start, stop)
  | none, none, some startLine, some stopLine =>
    if startLine > stopLine then
      throw s!"expected startLine <= stopLine, got {startLine} > {stopLine}"
    pure (← lineOffset source startLine, ← lineOffset source stopLine)
  | none, none, none, none =>
    throw "expected a non-empty exact line slice, or an explicit range"
  | _, _, _, _ =>
    throw "use exactly one complete range form: byte offsets (start/stop) or lines (startLine/stopLine)"

def resolveInferredLineRange (source body : String) (cfg : Config) : Except String (Nat × Nat × Nat × Nat) := do
  match cfg.start, cfg.stop, cfg.startLine, cfg.stopLine with
  | none, none, none, none => inferredLineRange source body cfg.occurrence
  | _, _, _, _ => throw "not an inferred line range"

def hasExplicitRange (cfg : Config) : Bool :=
  cfg.start.isSome || cfg.stop.isSome || cfg.startLine.isSome || cfg.stopLine.isSome

def withNl (source : String) : String :=
  if source.endsWith "\n" then source else source ++ "\n"

def backtickDelims (source : String) : String := Id.run do
  let mut n := 3
  let mut run := none
  let mut iter := source.startPos
  while h : iter ≠ source.endPos do
    let c := iter.get h
    iter := iter.next h
    if c == '`' then
      run := some (run.getD 0 + 1)
    else if let some k := run then
      if k > n then n := k
      run := none
  if let some k := run then
    if k > n then n := k
  n.fold (fun _ _ s => s.push '`') ""

def editCodeBlock (stx : Syntax) (newContents : String) : DocElabM (Option String) := do
  let txt ← getFileMap
  let some rng := stx.getRange?
    | pure none
  let { start := { line := l1, .. }, .. } := txt.utf8RangeToLspRange rng
  let line1 := (txt.lineStart (l1 + 1)).extract txt.source (txt.lineStart (l1 + 2))
  let line1ws := line1.takeWhile (· == ' ') |>.copy
  let line1rest := line1.drop line1ws.length
  if !line1rest.startsWith "```" then
    return none
  let opener := line1rest.dropWhile (· == '`') |>.trimAscii
  let delims := backtickDelims newContents
  let body := line1ws ++ (withNl newContents).replace "\n" ("\n" ++ line1ws)
  pure <| some s!"{line1ws}{delims}{opener}\n{withNl body}{line1ws}{delims}"

def replacementHint (original restated : String) : DocElabM MessageData := do
  let ref ← getRef
  let replacement? ← editCodeBlock ref original
  let replacement ← match replacement? with
    | some replacement => pure replacement
    | none =>
      let n := dedent.indentWidth restated |>.getD 2
      pure <| reindent original n
  MessageData.hint m!"Replace the restatement with the original:"
    #[{ suggestion := .string replacement, span? := some ref }]

def declArgSyntax? (args : Array Arg) : Option Syntax := Id.run do
  let mut seenPositional := 0
  for arg in args do
    match arg with
    | .anon value =>
      seenPositional := seenPositional + 1
      if seenPositional == 2 then
        return some value.syntax
    | .named .. | .flag .. => pure ()
  return none

def addStableRangeSuggestion (args : Array Arg) (cfg : Config) (startLine stopLine : Nat) : DocElabM Unit := do
  if !hasExplicitRange cfg then
    if let some declArg := declArgSyntax? args then
      let replacement := s!"{cfg.declName} (startLine := {startLine}) (stopLine := {stopLine})"
      Verso.Doc.Suggestion.saveSuggestion declArg "stable explicit range" replacement

def dropFenceTerminator (source : String) : String :=
  if source.endsWith "\n" then source.dropSuffix "\n" |>.copy else source

def normalizeRestated (expected restated : String) : String :=
  if restated == expected then restated else dropFenceTerminator restated

def expand (args : Array Arg) (cfg : Config) (str : StrLit) : DocElabM Term := do
  let env ← getEnv
  let items <- liftM (m := IO) <| loadModuleContentCached env ⟨cfg.project⟩ cfg.moduleName.toString
  let item <- match findItem items cfg.declName with
    | .ok item => pure item
    | .error err => throwErrorAt str err
  let commandCode <- match trimLeadingNewlines item.code with
    | .ok highlighted => pure highlighted
    | .error err => throwErrorAt str err
  let commandSource := commandCode.toString
  let rawRestated := dedent str.getString
  let (start, stop, stableStartLine?, stableStopLine?) ←
    match resolveInferredLineRange commandSource rawRestated cfg with
    | .ok (start, stop, startLine, stopLine) => pure (start, stop, some startLine, some stopLine)
    | .error _ =>
      match resolveInferredLineRange commandSource (dropFenceTerminator rawRestated) cfg with
      | .ok (start, stop, startLine, stopLine) => pure (start, stop, some startLine, some stopLine)
      | .error _ => match resolveRange commandSource rawRestated cfg with
      | .ok (start, stop) => pure (start, stop, none, none)
      | .error err =>
        let hint ← replacementHint commandSource rawRestated
        throwErrorAt str (m!"{err}" ++ hint)
  let expected <- match rawSlice commandSource start stop with
    | .ok source => pure source
    | .error err => throwErrorAt str err
  let restated := normalizeRestated expected rawRestated
  if restated != expected then
    let hint ← replacementHint expected restated
    throwErrorAt str (m!"recalled source slice does not match '{cfg.declName}'.\nExpected:{indentD expected}\nRestated:{indentD restated}" ++ hint)
  if let some startLine := stableStartLine? then
    if let some stopLine := stableStopLine? then
      addStableRangeSuggestion args cfg startLine stopLine
  let highlighted <- match crop commandCode start stop with
    | .ok highlighted => pure highlighted
    | .error err => throwErrorAt str err
  unless highlighted.toString == expected do
    throwErrorAt str "internal error: highlighted slice does not reproduce source bytes"
  ``(Verso.Doc.Block.other
    (Verso.Genre.Blog.BlockExt.highlightedCode
      (Verso.Genre.Blog.CodeOpts.mk $(quote cfg.moduleName) $(quote cfg.showProofStates))
      $(quote highlighted))
    #[Verso.Doc.Block.code $(quote expected)])

end Blog.LeanRecall

namespace Verso.Genre.Blog

@[code_block_expander leanRecall]
def leanRecall : CodeBlockExpander
  | args, str => do
    let cfg ← parseThe _root_.Blog.LeanRecall.Config args
    let block ← _root_.Blog.LeanRecall.expand args cfg str
    pure #[block]

end Verso.Genre.Blog
