import Verso.Doc.ArgParse

/-! Configuration for Typst role spans and fenced blocks. -/

open Lean Verso Doc ArgParse

namespace Blog.Typst

/-- Math input supplies formula contents; markup input supplies ordinary Typst source. -/
inductive Mode where
  | math
  | markup
  deriving Inhabited, BEq, Repr

/-- Interprets the `math` and `markup` argument values. -/
def Mode.ofName? : Name → Option Mode
  | `math => some .math
  | `markup => some .markup
  | _ => none

/-- The source syntax and display style of an embedded Typst fragment. -/
structure Config where
  mode : Mode
  /-- Selects block layout and, in math mode, a displayed equation. -/
  display : Bool

/-- Fenced-block options, defaulting to markup with block layout. -/
structure BlockConfig where
  toConfig : Config

/-- Inline math, the default for role spans. -/
def Config.inlineDefault : Config := ⟨.math, false⟩
/-- Block-level markup, the default for fenced blocks. -/
def Config.blockDefault : Config := ⟨.markup, true⟩

open Lean.Doc.Syntax in
private def displaySyntax (args : Array (TSyntax `doc_arg)) (fallback : Bool) : Bool := Id.run do
  for arg in args do
    match arg with
    | `(doc_arg| +$name:ident) => if name.getId == `display then return true
    | `(doc_arg| -$name:ident) => if name.getId == `display then return false
    | _ => pure ()
  for arg in args do
    match arg with
    | `(doc_arg| ($name:ident := $value:arg_val))
    | `(doc_arg| $name:ident := $value:arg_val) =>
      if name.getId == `display then
        match value with
        | `(arg_val| $value:ident) => return value.getId == `true || value.getId == ``true
        | _ => return false
    | _ => pure ()
  return fallback

open Lean.Doc.Syntax in
/-- Reads mode and display options from parsed syntax for IDE requests. -/
def Config.fromSyntax? (args : Array (TSyntax `doc_arg)) (defaults : Config) : Option Config := do
  let mut mode := defaults.mode
  for arg in args do
    match arg with
    | `(doc_arg| ($name:ident := $value:arg_val))
    | `(doc_arg| $name:ident := $value:arg_val) =>
      if name.getId == `mode then
        match value with
        | `(arg_val| $value:ident) => mode ← Mode.ofName? value.getId
        | _ => failure
        break
    | _ => pure ()
  return { mode, display := displaySyntax args defaults.display }

section
variable [Monad m] [MonadError m]

private def modeValue : ValDesc m Mode where
  description := "math or markup"
  signature := .Ident
  get
    | .name name =>
      match Mode.ofName? name.getId with
      | some mode => pure mode
      | none => throwErrorAt name "Expected 'math' or 'markup'"
    | value => throwError "Expected 'math' or 'markup', got {value}"

/-- Parses `mode`, `+display`, and `-display` with the supplied defaults. -/
def Config.parse (defaults : Config := .inlineDefault) : ArgParse m Config :=
  Config.mk <$> ((·.getD defaults.mode) <$> .named `mode modeValue true)
    <*> .flag `display defaults.display

instance : FromArgs Config m := ⟨Config.parse⟩
instance : FromArgs BlockConfig m := ⟨BlockConfig.mk <$> Config.parse .blockDefault⟩

end
end Blog.Typst
