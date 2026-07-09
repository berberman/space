import VersoBlog
import Verso.Doc.ArgParse
import Verso.Doc.Elab
import Verso.Parser

open Verso Genre Blog Doc ArgParse Lean Output Html Elab Parser Template

namespace Blog.Table

inductive Alignment where
  | left
  | center
  | right
deriving ToJson, FromJson, DecidableEq, Repr, Ord

def Alignment.htmlClass : Alignment → String
  | .left => "blog-table--left"
  | .center => "blog-table--center"
  | .right => "blog-table--right"

structure Config where
  header : Bool := false
  alignment : Option Alignment := none
deriving ToJson, FromJson

structure CellRange where
  startPos : String.Pos.Raw
  stopPos : String.Pos.Raw

def validateRows (ref : Syntax) (rows : Array (Array α)) : DocElabM Nat := do
  if rows.isEmpty then
    throwErrorAt ref "Expected at least one row"
  let columns := rows[0]!.size
  if columns == 0 then
    throwErrorAt ref "Expected at least one column"
  if rows.any (·.size != columns) then
    throwErrorAt ref s!"Expected all rows to have same number of columns, but got {rows.map (·.size)}"
  pure columns

def mkTable (cfg : Config) (columns : Nat) (cells : Array Term) : DocElabM Term := do
  let data := toJson (columns, cfg)
  ``(Block.other (Blog.BlockExt.component `Blog.Table.blogTable $(quote data)) #[Block.ul #[$[Verso.Doc.ListItem.mk #[$cells]],*]])

def parseRangeWith (p : ParserFn) (ref : Syntax) (startPos stopPos : String.Pos.Raw) : DocElabM Syntax := do
  let fileMap ← getFileMap
  if h : stopPos ≤ fileMap.source.rawEndPos then
    let ictx := mkInputContext fileMap.source (← getFileName) (endPos := stopPos) (endPos_valid := h)
    let env ← getEnv
    let state := { Lean.Parser.mkParserState fileMap.source with pos := startPos }
    let state := p.run ictx { env, options := ← getOptions } (Lean.Parser.getTokenTable env) state
    if let some err := state.errorMsg then
      throwErrorAt ref err.toString
    if state.recoveredErrors.size > 0 then
      throwErrorAt ref "Failed to parse table cell inline content"
    if ictx.atEnd state.pos then
      return state.stxStack.back
    else
      throwErrorAt ref "Unparsed input: `{state.pos.extract fileMap.source stopPos}`"
  else
    throwErrorAt ref "Internal error: table source range extends past end of file"

def sourceStopFromString (startPos : String.Pos.Raw) (s : String) : String.Pos.Raw :=
  ⟨startPos.byteIdx + s.utf8ByteSize⟩

def elabCell (ref : Syntax) (cell : CellRange) : DocElabM Term := do
  let inlines := (← parseRangeWith (textLine (allowNewlines := false)) ref cell.startPos cell.stopPos).getArgs
  if inlines.isEmpty then
    throwErrorAt ref "Expected nonempty table cell content"
  let genre := (← readThe DocElabContext).genreSyntax
  ``(Block.para (genre := $(⟨genre⟩)) #[$[$(← inlines.mapM fun inline => elabInline ⟨inline⟩)],*])

def trim (s : String) : String :=
  s.trimAscii.toString

def cell : ParserFn :=
  asStringFn <| takeWhileFn fun c => c != '|' && c != '\n'

def row : ParserFn :=
  nodeFn `Blog.Table.row <|
    takeWhileFn (· == ' ') >>
    ignoreFn (chFn '|') >>
    many1Fn (cell >> ignoreFn (chFn '|'))

def rows : ParserFn :=
  nodeFn `Blog.Table.rows <|
    row >> manyFn (ignoreFn (atomicFn (chFn '\n' >> lookaheadFn (takeWhileFn (· == ' ') >> chFn '|'))) >> row) >>
    takeWhileFn (·.isWhitespace)

def extractCell (ref : Syntax) : Syntax → DocElabM CellRange
  | .atom info cellText =>
    if trim cellText == "" then
      throwErrorAt ref "Table cells must be nonempty"
    else do
      let some startPos := info.getPos?
        | throwErrorAt ref "Internal error: table cell has no source position"
      let some stopPos := info.getTailPos?
        | throwErrorAt ref "Internal error: table cell has no end position"
      pure { startPos, stopPos }
  | stx => throwErrorAt stx "Expected table cell"

def extractRow (ref : Syntax) : Syntax → DocElabM (Array CellRange)
  | .node _ `Blog.Table.row #[.node _ `null cells] => cells.mapM <| extractCell ref
  | stx => throwErrorAt stx "Expected table row"

partial def extractRows (ref : Syntax) : Syntax → DocElabM (Array (Array CellRange))
  | .node _ `Blog.Table.rows #[first, rest] => do
    let first ← extractRow ref first
    let rest ← extractRows ref rest
    pure <| #[first] ++ rest
  | row@(.node _ `Blog.Table.row _) => return #[← extractRow ref row]
  | .node _ `null rows => rows.mapM <| extractRow ref
  | stx => throwErrorAt stx "Expected table row"

def elabRows (str : StrLit) : DocElabM (Array (Array Term)) := do
  let ref := str.raw
  let input := trim str.getString
  if input == "" then
    throwErrorAt ref "Expected at least one table row"
  let some startPos := ref.getPos?
    | throwErrorAt ref "Expected original table source positions"
  let some stopPos := ref.getTailPos?
    | throwErrorAt ref "Expected original table source end position"
  if _ : stopPos ≤ (← getFileMap).source.rawEndPos then
    let rowTexts ← extractRows ref (← parseRangeWith rows ref startPos stopPos)
    rowTexts.mapM fun cells => cells.mapM <| elabCell ref
  else
    throwErrorAt ref "Internal error: table source range extends past end of file"

section

variable [Monad m] [MonadError m]

def Config.parse : ArgParse m Config :=
  Config.mk <$> .flag `header true <*> .named `align alignment true
where
  alignment := {
    description := "Alignment of the table ('left', 'center', or 'right')"
    signature := .Ident
    get
      | .name x =>
        match x.getId with
        | `left => pure .left
        | `center => pure .center
        | `right => pure .right
        | _ => throwErrorAt x "Expected 'left', 'center', or 'right'"
      | .num x | .str x => throwErrorAt x "Expected 'left', 'center', or 'right'"
  }

instance : FromArgs Config m := ⟨Config.parse⟩

end

block_component blogTable where
  toHtml _id data _goI goB contents := do
    let (columns, cfg) ← match FromJson.fromJson? (α := Nat × Config) data with
      | .ok payload => pure payload
      | .error err =>
        reportError s!"Error deserializing table data: {err}"
        return .empty
    let alignClass := cfg.alignment.map (" " ++ ·.htmlClass) |>.getD ""
    let tableClass := "blog-table" ++ alignClass
    if let #[.ul items] := contents then
      if columns == 0 then
        reportError "Malformed table: expected positive column count"
        return .empty
      let mut items := items
      let mut rows := #[]
      while items.size > 0 do
        rows := rows.push (items.take columns |>.map (·.contents))
        items := items.extract columns items.size
      return {{
        <div class="table-scroll">
          <table class={{tableClass}}>
            {{← rows.mapIdxM fun i r => do
              let cols ← seq <$> r.mapM fun c => do
                let cell : Html ← c.mapM goB
                if cfg.header && i == 0 then
                  pure {{<th scope="col">{{cell}}</th>}}
                else
                  pure {{<td>{{cell}}</td>}}
              if cfg.header && i == 0 then
                pure {{<thead><tr>{{cols}}</tr></thead>}}
              else
                pure {{<tr>{{cols}}</tr>}}
            }}
          </table>
        </div>
      }}
    else
      reportError "Malformed table"
      return .empty

end Blog.Table

namespace Verso.Genre.Blog

@[code_block]
def table : CodeBlockExpanderOf _root_.Blog.Table.Config
  | cfg, str => do
    let rows ← _root_.Blog.Table.elabRows str
    let columns ← _root_.Blog.Table.validateRows str.raw rows
    _root_.Blog.Table.mkTable cfg columns rows.flatten

end Verso.Genre.Blog

open Verso Genre Blog
open Template
open Verso Doc Elab ArgParse
open Lean
open Verso Output Html
open Elab Command
open Lean.Doc.Syntax

@[role]
def typst : RoleExpanderOf Unit
  | (), contents => do
    let inl ← match contents with
      | #[inl] => pure inl
      | _ => throwError "Expected precisely one inline math, got {contents}"
    let html ← match inl with
      | `(inline| \math code($s)) => pure {{ <span class = "typst-inline"> {{Html.text false s.getString }} </span>}}
      | `(inline| \displaymath code($s)) => pure {{ <div class="typst-block"> {{Html.text false s.getString }} </div>}}
      | _ => throwErrorAt inl "Expected math code or displaymath code"
    `(_root_.Verso.Doc.Inline.other (Blog.InlineExt.blob $(quote html)) #[])

def mkLangCodeBlock (lang : String) (code : String) : Html :=
  {{<pre><code class=s!"language-{lang}">{{code}}</code></pre>}}

syntax "lang_code_block" ident : command

elab_rules : command
  | `(lang_code_block $lang) => do
    let cmd ← `(command|
                @[code_block]
                def $lang : CodeBlockExpanderOf Unit
                  | (), str => do
                    `(_root_.Verso.Doc.Block.other (Blog.BlockExt.blob (mkLangCodeBlock $(quote lang.getId.toString) $$str)) #[]))
    elabCommand cmd

lang_code_block python
lang_code_block haskell
lang_code_block kotlin
lang_code_block javascript
lang_code_block bash
lang_code_block coq
