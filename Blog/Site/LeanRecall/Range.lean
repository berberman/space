import Lean
import SubVerso.Highlighting.Split
import SubVerso.Module

open Lean
open SubVerso.Highlighting

namespace Blog.LeanRecall

def containsDecl (item : SubVerso.Module.ModuleItem) (declName : Name) : Bool :=
  item.defines.any (· == declName)

def findItem (items : Array SubVerso.Module.ModuleItem) (declName : Name) : Except String SubVerso.Module.ModuleItem := do
  let found := items.filter (containsDecl · declName)
  match found.size with
  | 0 => throw s!"declaration '{declName}' was not found in the extracted module"
  | 1 => pure found[0]!
  | _ => throw s!"declaration '{declName}' matched more than one extracted command"

def rawSlice (source : String) (start stop : Nat) : Except String String := do
  let startPos : String.Pos.Raw := ⟨start⟩
  let stopPos : String.Pos.Raw := ⟨stop⟩
  if startPos > stopPos then
    throw s!"expected start <= stop, got {start} > {stop}"
  if stopPos > source.rawEndPos then
    throw s!"slice end {stop} is past source end {source.rawEndPos.byteIdx}"
  if !startPos.isValid source then
    throw s!"slice start {start} is not a UTF-8 character boundary"
  if !stopPos.isValid source then
    throw s!"slice end {stop} is not a UTF-8 character boundary"
  pure <| String.Pos.Raw.extract source startPos stopPos

def sourceLines (source : String) : Array String :=
  let lines := source.splitOn "\n"
  lines.toArray.mapIdx fun i line =>
    if i + 1 == lines.length then line else line ++ "\n"

def lineStartOffsets (source : String) : Array Nat := Id.run do
  let mut out := #[0]
  let mut cursor := 0
  for line in sourceLines source do
    cursor := cursor + line.utf8ByteSize
    out := out.push cursor
  out

def lineOffset (source : String) (line : Nat) : Except String Nat := do
  if line == 0 then
    throw "line numbers are one-based"
  let starts := lineStartOffsets source
  if h : line - 1 < starts.size then
    pure starts[line - 1]
  else
    throw s!"line {line} is past the end of the source"

def dropFinalEmptyLine (lines : Array String) : Array String :=
  if lines.back? == some "" then lines.pop else lines

def bodyLines (body : String) : Array String :=
  if body.isEmpty then #[] else sourceLines body |> dropFinalEmptyLine

def sameLine (source body : String) (isLastBodyLine : Bool) : Bool :=
  source == body || isLastBodyLine && body.endsWith "\n" && source == body.dropSuffix "\n"

def linesMatchAt (source body : Array String) (start : Nat) : Bool := Id.run do
  if start + body.size > source.size then
    return false
  for h : i in [:body.size] do
    if !sameLine source[start + i]! body[i] (i + 1 == body.size) then
      return false
  return true

def findLineMatches (source body : Array String) : Array Nat := Id.run do
  let mut found := #[]
  for h : i in [:source.size] do
    if linesMatchAt source body i then
      found := found.push i
  found

def lineRangeFromIndex (source : String) (start lineCount : Nat) : Except String (Nat × Nat) := do
  pure (← lineOffset source (start + 1), ← lineOffset source (start + lineCount + 1))

def inferredLineRange (source body : String) (occurrence : Nat) : Except String (Nat × Nat × Nat × Nat) := do
  if occurrence == 0 then
    throw "occurrence is one-based"
  let body := bodyLines body
  if body.isEmpty then
    throw "block body is empty; use startLine/stopLine to recall an empty range"
  let found := findLineMatches (sourceLines source) body
  if h : occurrence - 1 < found.size then
    let startLine := found[occurrence - 1] + 1
    let stopLine := startLine + body.size
    let (start, stop) ← lineRangeFromIndex source found[occurrence - 1] body.size
    pure (start, stop, startLine, stopLine)
  else if found.isEmpty then
    throw "block body is not an exact line slice of the recalled declaration"
  else
    throw s!"block body occurs {found.size} times as a line slice in the recalled declaration; choose occurrence between 1 and {found.size}"

def cropLines (hl : Highlighted) (start stop : Nat) : Highlighted :=
  (hl.lines.extract start stop).foldl (· ++ ·) Highlighted.empty

def cropLeaf (mk : String -> Highlighted) (source : String) (start stop cursor : Nat) : Except String (Nat × Highlighted) := do
  let size := source.utf8ByteSize
  let leafStart := cursor
  let leafStop := cursor + size
  let cursor := leafStop
  let sliceStart := max start leafStart
  let sliceStop := min stop leafStop
  if sliceStart < sliceStop then
    let source <- rawSlice source (sliceStart - leafStart) (sliceStop - leafStart)
    pure (cursor, mk source)
  else
    pure (cursor, Highlighted.empty)

partial def cropAt (hl : Highlighted) (start stop cursor : Nat) : Except String (Nat × Highlighted) := do
  match hl with
  | .text source => cropLeaf .text source start stop cursor
  | .unparsed source => cropLeaf .unparsed source start stop cursor
  | .token tok =>
    cropLeaf (fun source => .token { tok with content := source }) tok.content start stop cursor
  | .seq highlights =>
    let mut cursor := cursor
    let mut out := Highlighted.empty
    for child in highlights do
      let (next, cropped) <- cropAt child start stop cursor
      cursor := next
      out := out ++ cropped
    pure (cursor, out)
  | .span info content =>
    let (cursor, cropped) <- cropAt content start stop cursor
    let out := if cropped.isEmpty then Highlighted.empty else .span info cropped
    pure (cursor, out)
  | .tactics _ _ _ content =>
    cropAt content start stop cursor
  | point@(.point ..) =>
    let out := if start <= cursor && cursor <= stop then point else Highlighted.empty
    pure (cursor, out)

def crop (hl : Highlighted) (start stop : Nat) : Except String Highlighted := do
  let source := hl.toString
  let _ <- rawSlice source start stop
  let (cursor, cropped) <- cropAt hl start stop 0
  if cursor != source.utf8ByteSize then
    throw s!"internal error: highlighted source length mismatch ({cursor} != {source.utf8ByteSize})"
  pure cropped

def leadingNewlineBytes (source : String) : Nat := Id.run do
  let mut bytes := 0
  for c in source.toList do
    if c == '\n' then
      bytes := bytes + 1
    else
      return bytes
  return bytes

def trimLeadingNewlines (hl : Highlighted) : Except String Highlighted :=
  let source := hl.toString
  let start := leadingNewlineBytes source
  if start == 0 then
    pure hl
  else
    crop hl start source.utf8ByteSize

def dedent (source : String) : String :=
  match indentWidth source with
  | some n =>
    "\n".intercalate <|
      source.splitOn "\n" |>.map fun line =>
        if line.toList.all (·.isWhitespace) then line else String.ofList (line.toList.drop n)
  | none => source
where
  indentWidth (source : String) : Option Nat :=
    source.splitOn "\n" |>.map (·.toList)
      |>.filter (fun line => !line.all (·.isWhitespace))
      |>.map (fun line => (line.takeWhile (· == ' ')).length)
      |>.min?

def reindent (source : String) (n : Nat) : String :=
  match source.splitOn "\n" with
  | [] => source
  | first :: rest =>
    let pad := String.ofList (List.replicate n ' ')
    "\n".intercalate (first :: rest.map fun line =>
      if line.toList.all (·.isWhitespace) then line else pad ++ line)

end Blog.LeanRecall
