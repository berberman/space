import VersoBlog

open Verso Genre Blog Site Doc

namespace Blog.CJKLineBreaks

private def inRange (n lo hi : Nat) : Bool :=
  decide (lo ≤ n ∧ n ≤ hi)

/--
An approximate "East Asian wide" predicate.

This is intentionally sufficient for Chinese/Japanese/Korean prose and
full-width punctuation rather than trying to implement all of UAX #11.
-/
def isEastAsianWide (c : Char) : Bool :=
  let n := c.toNat
  inRange n 0x1100 0x115f ||   -- Hangul Jamo
  inRange n 0x2e80 0xa4cf ||   -- CJK radicals, punctuation, kana, Han, etc.
  inRange n 0xac00 0xd7a3 ||   -- Hangul syllables
  inRange n 0xf900 0xfaff ||   -- CJK compatibility ideographs
  inRange n 0xfe10 0xfe19 ||   -- Vertical punctuation
  inRange n 0xfe30 0xfe6f ||   -- CJK compatibility/small forms
  inRange n 0xff01 0xff60 ||   -- Full-width forms
  inRange n 0xffe0 0xffe6 ||
  inRange n 0x20000 0x3fffd    -- Supplementary CJK ideographs

private def firstChar? (s : String) : Option Char :=
  s.foldl
    (fun found c =>
      match found with
      | some _ => found
      | none => some c)
    none

private def lastChar? (s : String) : Option Char :=
  s.foldl (fun _ c => some c) none


/-!
Find the first/last visible textual character of an inline.

Descending through `other` is useful for roles such as `{leanKw}`:
their visible representation normally lives in their child inlines.
-/

mutual

partial def firstVisibleChar {g : Genre} : Inline g → Option Char
  | .text s => firstChar? s
  | .code s => firstChar? s
  | .math _ s => firstChar? s
  | .emph xs => firstVisibleInlines xs
  | .bold xs => firstVisibleInlines xs
  | .link xs _ => firstVisibleInlines xs
  | .footnote _ xs => firstVisibleInlines xs
  | .concat xs => firstVisibleInlines xs
  | .other _ xs => firstVisibleInlines xs
  | .linebreak _ => none
  | .image _ _ => none

partial def firstVisibleInlines {g : Genre}
    (xs : Array (Inline g)) : Option Char :=
  go xs.toList
where
  go : List (Inline g) → Option Char
    | [] => none
    | x :: rest =>
      match firstVisibleChar x with
      | some c => some c
      | none => go rest

end


mutual

partial def lastVisibleChar {g : Genre} : Inline g → Option Char
  | .text s => lastChar? s
  | .code s => lastChar? s
  | .math _ s => lastChar? s
  | .emph xs => lastVisibleInlines xs
  | .bold xs => lastVisibleInlines xs
  | .link xs _ => lastVisibleInlines xs
  | .footnote _ xs => lastVisibleInlines xs
  | .concat xs => lastVisibleInlines xs
  | .other _ xs => lastVisibleInlines xs
  | .linebreak _ => none
  | .image _ _ => none

partial def lastVisibleInlines {g : Genre}
    (xs : Array (Inline g)) : Option Char :=
  go xs.toList
where
  go : List (Inline g) → Option Char
    | [] => none
    | x :: rest =>
      match go rest with
      | some c => some c
      | none => lastVisibleChar x

end


private def shouldDropBreak
    (before after : Option Char) : Bool :=
  match before, after with
  | some before, some after =>
    isEastAsianWide before && isEastAsianWide after
  | _, _ => false


/-!
First recursively normalize nested inline containers, then examine the
soft line breaks at this level.

Only an exact `"\n"` is treated as a removable soft break.
-/

mutual

partial def normalizeInline {g : Genre} : Inline g → Inline g
  | .text s => .text s
  | .code s => .code s
  | .math mode s => .math mode s
  | .linebreak s => .linebreak s
  | .image alt url => .image alt url
  | .emph xs => .emph (normalizeInlines xs)
  | .bold xs => .bold (normalizeInlines xs)
  | .link xs url => .link (normalizeInlines xs) url
  | .footnote name xs => .footnote name (normalizeInlines xs)
  | .concat xs => .concat (normalizeInlines xs)
  | .other ext xs => .other ext (normalizeInlines xs)

partial def normalizeInlines {g : Genre}
    (xs : Array (Inline g)) : Array (Inline g) :=
  let xs := xs.map normalizeInline
  (go none xs.toList).toArray
where
  go (before : Option Char) : List (Inline g) → List (Inline g)
    | [] => []

    | .linebreak s :: rest =>
      let after :=
        match rest with
        | next :: _ => firstVisibleChar next
        | [] => none

      if s == "\n" && shouldDropBreak before after then
        -- Drop the soft break, preserving adjacency.
        go before rest
      else
        -- Keeping whitespace breaks the adjacency, so reset `before`.
        .linebreak s :: go none rest

    | x :: rest =>
      -- If this inline has no visible textual edge, treat it as a boundary
      -- rather than accidentally joining CJK text across an image/opaque node.
      x :: go (lastVisibleChar x) rest

end


partial def normalizeBlock {g : Genre} : Block g → Block g
  | .para xs =>
    .para (normalizeInlines xs)

  | .code s =>
    .code s

  | .blockquote blocks =>
    .blockquote (blocks.map normalizeBlock)

  | .concat blocks =>
    .concat (blocks.map normalizeBlock)

  | .ul items =>
    .ul <| items.map fun ⟨blocks⟩ =>
      ⟨blocks.map normalizeBlock⟩

  | .ol start items =>
    .ol start <| items.map fun ⟨blocks⟩ =>
      ⟨blocks.map normalizeBlock⟩

  | .dl items =>
    .dl <| items.map fun ⟨term, desc⟩ =>
      ⟨normalizeInlines term, desc.map normalizeBlock⟩

  | .other ext blocks =>
    .other ext (blocks.map normalizeBlock)


partial def normalizePart {g : Genre} (part : Part g) : Part g :=
  {
    part with
    title := normalizeInlines part.title
    content := part.content.map normalizeBlock
    subParts := part.subParts.map normalizePart
  }


def normalizePost (post : BlogPost) : BlogPost :=
  { post with contents := normalizePart post.contents }


partial def normalizeDir : Dir → Dir
  | .page name id text contents =>
    .page name id
      (normalizePart text)
      (contents.map normalizeDir)

  | .blog name id text posts =>
    .blog name id
      (normalizePart text)
      (posts.map normalizePost)

  | .static name files =>
    .static name files


def run : Site → Site
  | .page id text contents =>
    .page id
      (normalizePart text)
      (contents.map normalizeDir)

  | .blog id text posts =>
    .blog id
      (normalizePart text)
      (posts.map normalizePost)

end Blog.CJKLineBreaks
