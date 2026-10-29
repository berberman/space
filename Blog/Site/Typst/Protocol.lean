import Lean.Data.Json

/-! Wire types for the newline-delimited JSON protocol between Lean and the Rust renderer. -/

open Lean

namespace Blog.Typst

/-- A complete synthetic Typst file submitted as one rendering batch. -/
structure Request where
  id : Nat
  source : String
  /-- Number of marked fragments in `source`. -/
  fragments : Nat
deriving ToJson

/-- A compiler message located by a synthetic UTF-8 byte range or a fragment index. -/
structure Diagnostic where
  /-- Compiler severity: `error` or `warning`. -/
  kind : String
  message : String
  start : Option Nat
  stop : Option Nat
  /-- Fragment index for page-boundary diagnostics. -/
  fragment : Option Nat
  hints : Array String
deriving FromJson, Repr

/-- Formats a compiler message and its hints for Lean diagnostics. -/
def Diagnostic.format (diag : Diagnostic) : String :=
  if diag.hints.isEmpty then s!"Typst: {diag.message}"
  else s!"Typst: {diag.message}\n" ++ String.intercalate "\n" (diag.hints.toList.map ("hint: " ++ ·))

/-- The indexed SVG outputs and compiler messages from a file-level batch. -/
structure Response where
  id : Nat
  /-- Whether the batch compiled and passed its page-boundary checks. -/
  success : Bool
  /-- `outputs[i]` renders fragment `i`; an empty string represents source with no visible output. -/
  outputs : Array String
  diagnostics : Array Diagnostic
deriving FromJson

end Blog.Typst
