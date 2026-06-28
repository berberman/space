import VersoBlog
import Verso.Doc.ArgParse
import Verso.Doc.Elab
import Verso.Parser

open Verso Genre Blog Doc ArgParse Lean Output Html Elab Parser

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

def validateRows (ref : Syntax) (rows : Array (Array α)) : DocElabM Nat := do
  if rows.isEmpty then
    throwErrorAt ref "Expected at least one row"
  let columns := rows[0]!.size
  if columns == 0 then
    throwErrorAt ref "Expected at least one column"
  if rows.any (·.size != columns) then
    throwErrorAt ref s!"Expected all rows to have same number of columns, but got {rows.map (·.size)}"
  pure columns

/- Elaborate verso blocks into syntax and put them into our block extension. -/
def mkTable (cfg : Config) (columns : Nat) (cells : Array (TSyntaxArray `block)) : DocElabM Term := do
  let blocks : Array (Syntax.TSepArray `term ",") ← cells.mapM (·.mapM elabBlock)
  let data := toJson (columns, cfg)
  ``(Block.other (Blog.BlockExt.component `Blog.Table.blogTable $(quote data)) #[Block.ul #[$[Verso.Doc.ListItem.mk #[$blocks,*]],*]])

/- Elaborate a cell to verso blocks. -/
def elabCell (ref : Syntax) (input : String) : DocElabM (TSyntaxArray `block) := do
  let raw := Syntax.mkStrLit input
  let blocks ← Concrete.stringToBlocks ⟨raw⟩
  if blocks.isEmpty then
    throwErrorAt ref "Expected nonempty table cell content"
  return blocks.map .mk

def trim (s : String) : String :=
  s.trimAscii.toString

def cell : ParserFn :=
  asStringFn <| takeWhileFn fun c => c != '|' && c != '\n'

/- Parse |...|...|...| returning the content in `Syntax.atom`. -/
def row : ParserFn :=
  nodeFn `Blog.Table.row <|
    ignoreFn (chFn '|') >>
    many1Fn (cell >> ignoreFn (chFn '|'))

/- Parse rows separated by newlines. -/
def rows : ParserFn :=
  nodeFn `Blog.Table.rows <|
    row >> manyFn (ignoreFn (chFn '\n') >> row)

/- Extract trimmed text from `.atom` node. -/
def extractCellText (ref : Syntax) : Syntax → DocElabM String
  | .atom _ cellText =>
    let cellText := trim cellText
    if cellText == "" then
      throwErrorAt ref "Table cells must be nonempty"
    else
      pure cellText
  | stx => throwErrorAt stx "Expected table cell"

/- Extract texts in a row (node created by `manyFn`). -/
partial def extractCells (ref : Syntax) : Syntax → DocElabM (Array String)
  | .node _ `null args => do
    let nested ← args.mapM <| extractCells ref
    pure nested.flatten
  | atom@(.atom ..) => do
    let cell ← extractCellText ref atom
    pure #[cell]
  | stx => throwErrorAt stx "Expected table cell"

/- Extract texts in a row (node created by `row`). -/
def extractRow (ref : Syntax) : Syntax → DocElabM (Array String)
  | .node _ `Blog.Table.row cells => do
    let nested ← cells.mapM <| extractCells ref
    pure nested.flatten
  | stx => throwErrorAt stx "Expected table row"

partial def extractRows (ref : Syntax) : Syntax → DocElabM (Array (Array String))
  | .node _ `Blog.Table.rows rows => do
    let nested ← rows.mapM <| extractRows ref
    pure nested.flatten
  | .node _ `Blog.Table.row cells => do
    let row ← extractRow ref <| .node SourceInfo.none `Blog.Table.row cells
    pure #[row]
  | .node _ `null rows => do
    let nested ← rows.mapM <| extractRows ref
    pure nested.flatten
  | stx => throwErrorAt stx "Expected table row"

/- Elaborate a table to nested verso blocks. -/
def elabRows (ref : Syntax) (input : String) : DocElabM (Array (Array (TSyntaxArray `block))) := do
  let input := trim input
  if input == "" then
    throwErrorAt ref "Expected at least one table row"
  let rowTexts ← extractRows ref (← rows.parseString input)
  rowTexts.mapM fun cells => cells.mapM <| elabCell ref

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
    let rows ← _root_.Blog.Table.elabRows str.raw str.getString
    let columns ← _root_.Blog.Table.validateRows str.raw rows
    _root_.Blog.Table.mkTable cfg columns rows.flatten

end Verso.Genre.Blog
