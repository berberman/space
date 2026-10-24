import VersoBlog
import Verso.Doc.ArgParse
import Verso.Doc.Elab
import Verso.Parser

import Blog.Site.Basic
import Blog.Site.LeanRecall
import Blog.Site.Typst

open Verso Genre Blog Doc ArgParse Lean Output Html Elab Parser Template

namespace Blog.OgImage

structure Config where
  url : String
  alt : String
  width : Option Nat := none
  height : Option Nat := none
  mimeType : Option String := none
deriving ToJson, FromJson

def marker (cfg : Config) : Html :=
  .tag "span" (#[("class", "blog-og-image"), ("data-url", cfg.url), ("data-alt", cfg.alt)] ++
    (cfg.width.map fun width => [("data-width", toString width)]).getD [] ++
    (cfg.height.map fun height => [("data-height", toString height)]).getD [] ++
    (cfg.mimeType.map fun mimeType => [("data-type", mimeType)]).getD []) Html.empty

block_component ogImageMarker (cfg : Config) where
  toHtml _id _data _goI _goB _contents := pure <| marker cfg

section

variable [Monad m] [MonadError m]

def Config.parse : ArgParse m Config :=
  Config.mk <$>
    .positional `url .string <*>
    .positional `alt .string <*>
    .named `width .nat true <*>
    .named `height .nat true <*>
    .named `type .string true

instance : FromArgs Config m := ⟨Config.parse⟩

end


end Blog.OgImage

namespace Blog.Afilmory

structure Config where
  photoId : String
  width : Nat
  height : Nat
  embedWidth : String := "100%"
  useAsOgImage : Bool := false
  alt : String := "Afilmory photo"
  caption : Option String := none
deriving ToJson, FromJson

def thumbnailUrl (photoId : String) : String :=
  s!"{Blog.SiteConfig.afilmoryBaseUrl}/thumbnails/{photoId}.jpg"

block_component afilmoryEmbed (cfg : Config) where
  toHtml _id _data _goI _goB _contents := do
    let aspect := s!"{cfg.width}:{cfg.height}"
    let embedScriptUrl := s!"{Blog.SiteConfig.afilmoryBaseUrl}/share/embed.js"
    let ogMarker := if cfg.useAsOgImage then
        Blog.OgImage.marker {
          url := thumbnailUrl cfg.photoId
          alt := cfg.alt
          mimeType := some "image/jpeg"
        }
      else
        Html.empty
    let caption := (cfg.caption.map fun text => {{<figcaption>{{text}}</figcaption>}}).getD Html.empty
    pure <| ogMarker ++ {{
      <figure class="afilmory-embed">
        <script async src={{embedScriptUrl}}
                data-afilmory-photo={{cfg.photoId}}
                data-aspect={{aspect}}
                data-width={{cfg.embedWidth}}></script>
        {{caption}}
      </figure>
    }}

section

variable [Monad m] [MonadError m]

def Config.parse : ArgParse m Config :=
  Config.mk <$>
    .positional `photo .string <*>
    .positional `width .nat <*>
    .positional `height .nat <*>
    .namedD `embedWidth .string "100%" <*>
    .flag `og true <*>
    .namedD `alt .string "Afilmory photo" <*>
    .named `caption .string true

instance : FromArgs Config m := ⟨Config.parse⟩

end


end Blog.Afilmory

namespace Verso.Genre.Blog

@[directive]
def ogImage : DirectiveExpanderOf _root_.Blog.OgImage.Config
  | cfg, contents => do
    if h : contents.size > 0 then
      logErrorAt contents[0] "Expected no contents"
    ``(_root_.Blog.OgImage.ogImageMarker
      (_root_.Blog.OgImage.Config.mk
        $(quote cfg.url)
        $(quote cfg.alt)
        $(quote cfg.width)
        $(quote cfg.height)
        $(quote cfg.mimeType)) #[])

@[directive]
def afilmory : DirectiveExpanderOf _root_.Blog.Afilmory.Config
  | cfg, contents => do
    if h : contents.size > 0 then
      logErrorAt contents[0] "Expected no contents"
    ``(_root_.Blog.Afilmory.afilmoryEmbed
      (_root_.Blog.Afilmory.Config.mk
        $(quote cfg.photoId)
        $(quote cfg.width)
        $(quote cfg.height)
        $(quote cfg.embedWidth)
        $(quote cfg.useAsOgImage)
        $(quote cfg.alt)
        $(quote cfg.caption)) #[])

end Verso.Genre.Blog

namespace Blog.Table

inductive Alignment where
  | left
  | center
  | right
deriving ToJson, FromJson, DecidableEq, Repr, Ord, Quote

def Alignment.htmlClass : Alignment → String
  | .left => "blog-table--left"
  | .center => "blog-table--center"
  | .right => "blog-table--right"

structure Config where
  header : Bool := false
  alignment : Option Alignment := none
deriving ToJson, FromJson, Quote

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
  ``(Block.other (Blog.BlockExt.component `Blog.Table.blogTable (toJson ($(quote columns), $(quote cfg)))) #[Block.ul #[$[Verso.Doc.ListItem.mk #[$cells]],*]])

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
