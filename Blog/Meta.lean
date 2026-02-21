import VersoBlog

open Verso Genre Blog
open Template
open Verso Doc Elab ArgParse
open Lean
open Verso Output Html
open Template
open Elab Command
open Lean.Doc.Syntax

def runTypstToMathML (blockMode : Bool) (content : String) : IO String :=
  IO.Process.run {
    cmd := "python3",
    args := let args0 := #["typst/typst_to_mathml.py"]
            let args1 := if blockMode then args0.push "--block" else args0
            args1.push content
  }

@[role]
def typst : RoleExpanderOf Unit
  | (), contents => do
    let #[inline] := contents
      | throwError "Expected precisely one inline math, got {contents}"
    let (s, block) ← match inline with
      | `(inline| \math code($s)) => pure (s, false)
      | `(inline| \displaymath code($s)) => pure (s, true)
      | _ => throwErrorAt inline "Expected math code or displaymath code"
    let content ← runTypstToMathML block s.getString
    let html := Html.text false content
    `(Inline.other (Blog.InlineExt.blob $(quote html)) #[])

def mkLangCodeBlock (lang : String) (code : String) : Html :=
  {{<pre><code class=s!"language-{lang}">{{code}}</code></pre>}}

syntax "lang_code_block" ident : command

elab_rules : command
  | `(lang_code_block $lang) => do
    let cmd ← `(command|
                @[code_block]
                def $lang : CodeBlockExpanderOf Unit
                  | (), str => do
                    `(Block.other (Blog.BlockExt.blob (mkLangCodeBlock $(quote lang.getId.toString) $$str)) #[]))
    elabCommand cmd

lang_code_block python
lang_code_block haskell
lang_code_block kotlin
lang_code_block javascript
lang_code_block bash
