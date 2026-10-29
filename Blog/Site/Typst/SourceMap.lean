import Verso.Parser
import Lean.Data.Lsp.Utf16
import Lean.Data.Json
import Blog.Site.Typst.Config
import Verso.Doc.Concrete

/-! Synthetic Typst files and position mapping between Lean source, decoded fragments, and LSP ranges. -/

open Lean Lean.Doc.Syntax

namespace Blog.Typst.Ide

/-- Byte offsets for a deindented line, paired with its original Lean position. -/
structure SourceLine where
  offset : Nat
  hostStart : String.Pos.Raw
  length : Nat
  deriving Inhabited, Repr

/-- Typst source from a Verso span or fence, together with its Lean source map. -/
structure Fragment where
  start : String.Pos.Raw
  stop : String.Pos.Raw
  /-- Decoded source passed to Typst. -/
  source : String
  display : Bool
  mode : Blog.Typst.Mode := .math
  /-- Per-line offsets for deindented fences; empty for contiguous source. -/
  lines : Array SourceLine := #[]
  /-- Leading spaces restored in multiline completion edits. -/
  indentation : String := ""
  /-- Whether decoded byte offsets have exact Lean source positions. -/
  exact : Bool := true
  deriving Inhabited, Repr

private def Fragment.sourceOffset? (fragment : Fragment) (pos : String.Pos.Raw) : Option Nat := do
  if !fragment.exact then failure
  if fragment.lines.isEmpty then
    if pos.byteIdx < fragment.start.byteIdx || pos.byteIdx > fragment.stop.byteIdx then none
    else some (pos.byteIdx - fragment.start.byteIdx)
  else
    for line in fragment.lines do
      if line.hostStart.byteIdx ≤ pos.byteIdx && pos.byteIdx ≤ line.hostStart.byteIdx + line.length then
        return line.offset + pos.byteIdx - line.hostStart.byteIdx
    none

private def Fragment.hostPosition? (fragment : Fragment) (offset : Nat) : Option String.Pos.Raw := do
  if !fragment.exact then failure
  if offset > fragment.source.utf8ByteSize then failure
  if fragment.lines.isEmpty then return fragment.start.increaseBy offset
  for i in [:fragment.lines.size] do
    let line := fragment.lines[fragment.lines.size - 1 - i]!
    if line.offset ≤ offset && offset ≤ line.offset + line.length then
      return line.hostStart.increaseBy (offset - line.offset)
  none

/-- Maps a fragment-relative UTF-8 byte range to Lean syntax. -/
def Fragment.ref? (fragment : Fragment) (start stop : Nat) : Option Syntax := do
  if start > stop then failure
  let start ← fragment.hostPosition? start
  let stop ← fragment.hostPosition? stop
  return Syntax.ofRange { start, stop }

/-- Extracts decoded source and its exact Lean positions from a Verso code string. -/
def fragmentOfSyntax? (text : FileMap) (str : StrLit) (config : Blog.Typst.Config) : Option Fragment := do
  let start ← str.raw.getPos?
  let stop ← str.raw.getTailPos?
  if stop.byteIdx > text.source.utf8ByteSize then failure
  let source := str.getString
  let raw := start.extract text.source stop
  let fragment : Fragment := { start, stop, source, display := config.display, mode := config.mode }
  if raw == source then return fragment
  let rawLines := raw.splitOn "\n" |>.toArray
  let sourceLines := source.splitOn "\n" |>.toArray
  if raw.isEmpty && source == "\n" then
    return { fragment with lines := #[{ offset := 0, hostStart := start, length := 0 }] }
  if rawLines.size != sourceLines.size then failure
  let mut lines := #[]
  let mut sourceOffset := 0
  let mut hostOffset := start.byteIdx
  let mut indentation := 0
  for i in [:sourceLines.size] do
    let original := rawLines[i]!
    let decoded := sourceLines[i]!
    if original.utf8ByteSize < decoded.utf8ByteSize then failure
    let removed := original.utf8ByteSize - decoded.utf8ByteSize
    if (original.take removed).toString.any (· != ' ') || (original.drop removed).toString != decoded then
      failure
    indentation := max indentation removed
    let length := decoded.utf8ByteSize + if i + 1 < sourceLines.size then 1 else 0
    lines := lines.push { offset := sourceOffset, hostStart := ⟨hostOffset + removed⟩, length }
    sourceOffset := sourceOffset + length
    hostOffset := hostOffset + original.utf8ByteSize + 1
  return { fragment with lines, indentation := "".pushn ' ' indentation }

/-- Extracts decoded source with either an exact map or a whole-span location. -/
def fragmentOfSyntax (text : FileMap) (str : StrLit) (config : Blog.Typst.Config) : Fragment :=
  match fragmentOfSyntax? text str config with
  | some fragment => fragment
  | none => {
    start := str.raw.getPos?.getD 0
    stop := str.raw.getTailPos?.getD (str.raw.getPos?.getD 0)
    source := str.getString
    display := config.display
    mode := config.mode
    exact := false
  }

/-- Recognizes Typst spans and fences, returning their source string, arguments, and defaults. -/
def fragmentSyntax? (stx : Syntax) : Option (StrLit × Array (TSyntax `doc_arg) × Blog.Typst.Config) := do
  if let `(inline| role{ $name:ident $args:doc_arg* }[ $contents:inline* ]) := stx then
    if name.getId == `typst then
      if let #[inl] := contents then
        if let `(inline| code( $source:str )) := inl then
          return (source, args, .inlineDefault)
  if let `(block| ``` $name:ident $args:doc_arg* | $source:str ```) := stx then
    if name.getId == `typst then return (source, args, .blockDefault)
  none

private def fragmentAtNode? (text : FileMap) (stx : Syntax) : Option Fragment := do
  let (source, args, defaults) ← fragmentSyntax? stx
  let config ← Blog.Typst.Config.fromSyntax? args defaults
  return fragmentOfSyntax text source config

/-- A file-local fragment index and the Verso document that owns its scope. -/
structure ScopedFragment where
  id : Nat
  /-- Byte position of the root Verso document. -/
  document : Nat
  fragment : Fragment
  deriving Inhabited

/-- A fragment's placement in the synthetic Typst file. -/
structure DocumentSegment extends ScopedFragment where
  /-- UTF-8 byte offset of the fragment's source in `Document.source`. -/
  offset : Nat
  deriving Inhabited

/-- The complete synthetic Typst file and its fragment lookup indexes. -/
structure Document where
  source : String
  /-- Fragments ordered by synthetic byte offset. -/
  segments : Array DocumentSegment
  /-- Fragments ordered by Lean source position. -/
  hostSegments : Array DocumentSegment
  /-- Fragments indexed by file-local ID. -/
  segmentsById : Array (Option DocumentSegment)
  /-- Cached LSP coordinate map for `source`. -/
  text : FileMap

private def Fragment.opening (fragment : Fragment) : String :=
  match fragment.mode with
  | .math => if fragment.display then "$ " else "$"
  | .markup => ""

private def Fragment.closing (fragment : Fragment) : String :=
  match fragment.mode with
  | .math => if fragment.display then " $" else "$"
  | .markup => ""

/-- Builds the synthetic file with document scopes and marked fragment boundaries. -/
def Document.create (entries : Array ScopedFragment) : Document := Id.run do
  let entries := entries.qsort fun a b =>
    a.document < b.document || (a.document == b.document && a.fragment.start < b.fragment.start)
  let mut source := include_str "../../../typst/preamble.typ"
  let mut segments := #[]
  let mut scope : Option Nat := none
  for entry in entries do
    if scope != some entry.document then
      if scope.isSome then source := source ++ "\n]\n#std.pagebreak()\n"
      source := source ++ "\n#[\n"
      scope := some entry.document
    else source := source ++ "\n#std.pagebreak()\n"
    source := source ++ s!"#std.metadata(none) <__space-typst-start-{entry.id}>\n" ++ entry.fragment.opening
    segments := segments.push { toScopedFragment := entry, offset := source.utf8ByteSize }
    source := source ++ entry.fragment.source ++ entry.fragment.closing ++
      s!"\n#std.metadata(none) <__space-typst-end-{entry.id}>\n"
  if scope.isSome then source := source ++ "\n]\n"
  let hostSegments := segments.qsort (·.fragment.start < ·.fragment.start)
  let segmentsById := segments.foldl (fun table segment => table.set! segment.id (some segment))
    (Array.replicate entries.size none)
  return { source, segments, hostSegments, segmentsById, text := FileMap.ofString source }

@[inline] private def upperBound (size : Nat) (key : Nat → Nat) (value : Nat) : Nat := Id.run do
  let mut low := 0
  let mut high := size
  while low < high do
    let mid := low + (high - low) / 2
    if key mid ≤ value then low := mid + 1 else high := mid
  return low

/-- Finds the fragment whose exact source map contains a Lean cursor position. -/
def Document.segmentAt? (doc : Document) (pos : String.Pos.Raw) : Option DocumentSegment := do
  let index := upperBound doc.hostSegments.size (fun i => doc.hostSegments[i]!.fragment.start.byteIdx) pos.byteIdx
  if index == 0 then failure
  let segment ← doc.hostSegments[index - 1]?
  let _ ← segment.fragment.sourceOffset? pos
  return segment

/-- Maps a synthetic byte range to Lean syntax, using whole-span locations for unmapped fragments. -/
def Document.ref? (doc : Document) (start stop : Nat) : Option Syntax := do
  if start > stop then failure
  let index := upperBound doc.segments.size (fun i => doc.segments[i]!.offset) start
  if index == 0 then failure
  let segment ← doc.segments[index - 1]?
  if stop > segment.offset + segment.fragment.source.utf8ByteSize then failure
  segment.fragment.ref? (start - segment.offset) (stop - segment.offset) <|>
    if !segment.fragment.exact then some <| Syntax.ofRange {
      start := segment.fragment.start, stop := segment.fragment.stop }
    else none

/-- Maps a Lean byte position to a UTF-16 LSP position in the synthetic file. -/
def Document.virtualPosition (doc : Document) (segment : DocumentSegment) (pos : String.Pos.Raw) : Lsp.Position :=
  doc.text.utf8PosToLspPos
    ⟨segment.offset + (segment.fragment.sourceOffset? pos).getD 0⟩

private def isCompleteDocument (stx : Syntax) : Bool :=
  stx.getKind == ``Verso.Doc.Concrete.docTerm ||
    stx.getArgs.any (fun child => child.isAtom && (child.getAtomVal == "#docs" || child.getAtomVal == "#doc"))

/-- Selects the title string of a `#doc`, `#docs`, or `verso` document. -/
def documentTitle? (stx : Syntax) : Option StrLit := do
  if !isCompleteDocument stx then failure
  let title ← stx.getArgs.find? (·.isOfKind strLitKind)
  return ⟨title⟩

private def titleInlines? (env : Environment) (text : FileMap) (stx : Syntax) : Option Syntax := do
  let title ← documentTitle? stx
  let start ← title.raw.getPos?
  let stop ← title.raw.getTailPos?
  let (decoded, positions) := Verso.SyntaxUtils.decodeStrLitWithMap text.source start stop
  if decoded != title.getString then failure
  let state := (Verso.Parser.textLine true).run
    (Parser.mkInputContext decoded "<document title>") { env, options := {} }
    (Parser.getTokenTable env) (Parser.mkParserState decoded)
  if !state.allErrors.isEmpty || state.stxStack.size != 1 then failure
  return Verso.SyntaxUtils.remapSyntaxPos text.source
    (Verso.SyntaxUtils.mapDecodedPos positions) state.stxStack.back

private def contains (stx : Syntax) (pos : String.Pos.Raw) : Bool :=
  match stx.getRange? with
  | some range => range.start ≤ pos && pos ≤ range.stop
  | none => false

/-- Checks whether a cursor is inside Typst source, including code in document titles. -/
partial def containsTypst (env : Environment) (text : FileMap) (stx : Syntax) (pos : String.Pos.Raw) : Bool :=
  if let some (source, _, _) := fragmentSyntax? stx then contains source.raw pos
  else
    let inTitle := do
      let title ← documentTitle? stx
      if !contains title.raw pos then failure
      let inlines ← titleInlines? env text stx
      return containsTypst env text inlines pos
    inTitle.getD false || stx.getArgs.any (containsTypst env text · pos)

private partial def collectScoped (env : Environment) (text : FileMap) (stx : Syntax) (owner : Option Nat)
    (entries : Array ScopedFragment) : Array ScopedFragment := Id.run do
  let owner := if isCompleteDocument stx then stx.getPos?.map (·.byteIdx) else owner
  if let some owner := owner then
    if let some fragment := fragmentAtNode? text stx then
      return entries.push { id := entries.size, document := owner, fragment }
  let mut entries := entries
  if let some title := titleInlines? env text stx then
    entries := collectScoped env text title owner entries
  for child in stx.getArgs do
    entries := collectScoped env text child owner entries
  return entries

/-- Collects Typst fragments from parsed Lean commands, including document titles. -/
def fileDocument (env : Environment) (text : FileMap) (commands : Array Syntax) : Document := Id.run do
  let mut owner := none
  let mut entries := #[]
  for command in commands do
    if command.getKind == ``Verso.Doc.Concrete.replaceDoc then
      owner := command.getPos?.map (·.byteIdx)
      entries := collectScoped env text command owner entries
    else if command.getKind == ``Verso.Doc.Concrete.addBlockCmd ||
        command.getKind == ``Verso.Doc.Concrete.addLastBlockCmd then
      entries := collectScoped env text command owner entries
    else
      owner := none
      entries := collectScoped env text command none entries
  return Document.create entries

private def eraseField (json : Json) (field : String) : Json :=
  match json with
  | .obj fields => .obj (fields.erase field)
  | _ => json

private def field? (json : Json) (field : String) : Option Json :=
  (json.getObjVal? field).toOption

private def exactPosition (text : FileMap) (json : Json) : Except String String.Pos.Raw := do
  let pos : Lsp.Position ← fromJson? json
  let some start := text.positions[pos.line]? | throw "Position is outside the virtual document"
  let stop := text.positions[pos.line + 1]?.getD text.source.rawEndPos
  if pos.character > (start.extract text.source stop).utf16Length then
    throw "Position is beyond the end of the line"
  let raw := text.lspPosToUtf8Pos pos
  if text.utf8PosToLspPos raw != pos then
    throw "Position is not a UTF-16 character boundary"
  return raw

private structure Mapping where
  fragment : Fragment
  host : FileMap
  virtual : FileMap
  offset : Nat

private def Document.mapping (doc : Document) (segment : DocumentSegment) (host : FileMap) : Mapping :=
  { fragment := segment.fragment, host, virtual := doc.text,
    offset := segment.offset }

private def Mapping.position (mapping : Mapping) (json : Json) : Except String String.Pos.Raw := do
  let raw ← exactPosition mapping.virtual json
  if raw.byteIdx < mapping.offset ||
      raw.byteIdx > mapping.offset + mapping.fragment.source.utf8ByteSize then
    throw "Range extends outside the Typst fragment"
  match mapping.fragment.hostPosition? (raw.byteIdx - mapping.offset) with
  | some host => return host
  | none => throw "Range extends outside the host fragment"

private def Mapping.range (mapping : Mapping) (json : Json) : Except String Json := do
  let _ ← json.getObj?
  let start ← mapping.position (← json.getObjVal? "start")
  let stop ← mapping.position (← json.getObjVal? "end")
  if start.byteIdx > stop.byteIdx then throw "Range is reversed"
  return json.setObjVal! "start" (toJson (mapping.host.utf8PosToLspPos start))
    |>.setObjVal! "end" (toJson (mapping.host.utf8PosToLspPos stop))

/-- Translates a Tinymist hover range into the Lean file's coordinates. -/
def Document.mapHover (doc : Document) (segment : DocumentSegment) (host : FileMap) (json : Json) : Except String Json := do
  if json.isNull then return json
  let _ ← json.getObj?
  let some range := field? json "range" | return json
  match (doc.mapping segment host).range range with
  | .ok range => return json.setObjVal! "range" range
  | .error _ => return eraseField json "range"

private def Mapping.editRange (mapping : Mapping) (json : Json) : Except String Json := do
  if (field? json "start").isSome then return ← mapping.range json
  let insert ← mapping.range (← json.getObjVal? "insert")
  let replace ← mapping.range (← json.getObjVal? "replace")
  return json.setObjVal! "insert" insert |>.setObjVal! "replace" replace

private def Mapping.editText (mapping : Mapping) (edit : Json) : Except String Json := do
  let text ← edit.getObjValAs? String "newText"
  if mapping.fragment.indentation.isEmpty then return edit
  return edit.setObjVal! "newText" (toJson <| text.replace "\n" ("\n" ++ mapping.fragment.indentation))

private def Mapping.textEdit (mapping : Mapping) (json : Json) : Except String Json := do
  let json ← mapping.editText json
  if let some range := field? json "range" then
    let range ← mapping.range range
    return (eraseField json "range").setObjVal! "insert" range
      |>.setObjVal! "replace" range
  mapping.editRange json

private def Mapping.completionItem (mapping : Mapping) (uri : Lsp.DocumentUri)
    (json : Json) : Except String Json := do
  let _ ← json.getObj?
  let mut item := json
  if let some edit := field? item "textEdit" then
    item := item.setObjVal! "textEdit" (← mapping.textEdit edit)
  if let some edits := field? item "additionalTextEdits" then
    let edits ← edits.getArr?
    let edits ← edits.mapM fun edit => do
      let edit ← mapping.editText edit
      let range ← mapping.range (← edit.getObjVal? "range")
      return edit.setObjVal! "range" range
    item := item.setObjVal! "additionalTextEdits" (.arr edits)
  if let some (.str documentation) := field? item "documentation" then
    item := item.setObjVal! "documentation"
      (Json.mkObj [("kind", .str "markdown"), ("value", .str documentation)])
  return item.setObjVal! "data"
    (Json.mkObj [("uri", toJson uri), ("typstIde", .bool true)])

private def Mapping.completionItems (mapping : Mapping) (uri : Lsp.DocumentUri)
    (items : Array Json) : Array Json :=
  items.filterMap (fun item => (mapping.completionItem uri item).toOption)

/-- Translates Tinymist completion edits and restores fence indentation. -/
def Document.mapCompletion (doc : Document) (segment : DocumentSegment) (host : FileMap)
    (uri : Lsp.DocumentUri) (json : Json) : Except String Json := do
  let mapping := doc.mapping segment host
  match json with
  | .null => return .null
  | .arr items => return .arr (mapping.completionItems uri items)
  | .obj _ =>
    if (field? json "itemDefaults").isSome then throw "Tinymist returned unnegotiated completion defaults"
    let items ← (← json.getObjVal? "items").getArr?
    return json.setObjVal! "items" (.arr (mapping.completionItems uri items))
  | _ => throw "Expected null, a completion array, or a completion list"

end Blog.Typst.Ide
