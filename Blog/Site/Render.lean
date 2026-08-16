import Blog.Site.Basic

open Verso Genre Blog Site Syntax Doc Output Html

namespace Blog.HeadingLinks

/-- Add a quiet `#` permalink to every heading that already has an `id`. -/
partial def run (pagePath : String) : Html → Html
  | .text escape str => .text escape str
  | .seq contents => .seq (contents.map (run pagePath))
  | .tag name attrs contents =>
    let contents := run pagePath contents
    if ["h2", "h3", "h4", "h5", "h6"].contains name then
      match attr? "id" attrs with
      | some id =>
        .tag name attrs (contents ++ {{<a class="heading-link" href=s!"{pagePath}#{id}" aria-label="Link to this section">"#"</a>}})
      | none => .tag name attrs contents
    else
      .tag name attrs contents

end Blog.HeadingLinks

namespace Blog.Footnotes

abbrev Note := Html
abbrev M := StateM (Array Note)

def isFootnote (name : String) (attrs : Array (String × String)) : Bool :=
  name == "details" && attr? "class" attrs == some "footnote"

def stripSummary : Html → Html
  | .seq contents =>
    match contents.toList with
    | .tag "summary" _ _ :: rest => .seq rest.toArray
    | _ => .seq contents
  | html => html

def ref (pagePath : String) (number : Nat) : Html :=
  {{<sup id=s!"fnref-{number}" class="footnote-ref"><a href=s!"{pagePath}#fn-{number}" aria-label=s!"Footnote {number}">{{toString number}}</a></sup>}}

def summaryRef (target : String) (number : Nat) : Html :=
  {{<sup class="footnote-ref"><a href=s!"{target}/#fn-{number}" aria-label=s!"Footnote {number}">{{toString number}}</a></sup>}}

def item (pagePath : String) (number : Nat) (body : Html) : Html :=
  {{<li id=s!"fn-{number}">{{body}} <a class="footnote-backref" href=s!"{pagePath}#fnref-{number}" aria-label=s!"Back to footnote {number} reference">"↩"</a></li>}}

def items (pagePath : String) (notes : Array Note) : Array Html :=
  notes.foldl (init := (#[], 1)) (fun (rendered, number) body =>
    (rendered.push (item pagePath number body), number + 1)) |>.1

def renderSection (pagePath : String) (notes : Array Note) : Html :=
  {{<section id="footnotes" class="footnotes" aria-label="Footnotes"><hr/><ol>{{items pagePath notes}}</ol></section>}}

partial def collect (pagePath : String) : Html → M Html
  | .text escape str => return .text escape str
  | .seq contents => return .seq (← contents.mapM <| collect pagePath)
  | .tag name attrs contents => do
    if isFootnote name attrs then
      let notes ← get
      let number := notes.size + 1
      modify (·.push Html.empty)
      let body ← collect pagePath <| stripSummary contents
      modify (·.set! (number - 1) body)
      return ref pagePath number
    else
      return .tag name attrs (← collect pagePath contents)

def run (pagePath : String) (content : Html) : Html :=
  let (content, notes) := (collect pagePath content).run #[]
  if notes.isEmpty then content else content ++ renderSection pagePath notes

partial def summary (target : String) : Html → StateM Nat Html
  | .text escape str => return .text escape str
  | .seq contents => return .seq (← contents.mapM <| summary target)
  | .tag name attrs contents => do
    if isFootnote name attrs then
      let number ← get
      modify (· + 1)
      return summaryRef target number
    else
      return .tag name attrs (← summary target contents)

def runSummary (target : String) (content : Html) : Html := summary target content |>.run' 1

end Blog.Footnotes

namespace Blog.SectionSidebar

structure Item where
  level : Nat
  id : String
  label : String

def headingLevel? : String → Option Nat
  | "h2" => some 2
  | "h3" => some 3
  | "h4" => some 4
  | "h5" => some 5
  | "h6" => some 6
  | _ => none

def isHeadingLink (attrs : Array (String × String)) : Bool :=
  attr? "class" attrs == some "heading-link"

partial def labelText : Html → String
  | .text _ str => str
  | .seq contents => contents.foldl (fun out html => out ++ labelText html) ""
  | .tag "a" attrs contents =>
    if isHeadingLink attrs then "" else labelText contents
  | .tag _ _ contents => labelText contents

partial def items : Html → Array Item
  | .text _ _ => #[]
  | .seq contents => contents.foldl (fun out html => out ++ items html) #[]
  | .tag name attrs contents =>
    match headingLevel? name, attr? "id" attrs with
    | some level, some id =>
      let label := normalizeSpaces (labelText contents)
      if label.isEmpty then #[] else #[{ level, id, label }]
    | _, _ => items contents

def itemHtml (pagePath : String) (item : Item) : Html :=
  {{<li class=s!"section-sidebar__item section-sidebar__item--h{item.level}"><a href=s!"{pagePath}#{item.id}" data-section-id={{item.id}}>{{item.label}}</a></li>}}

def render (pagePath : String) (title : String) (content : Html) : Html :=
  let found := items content
  if found.size < 2 then
    Html.empty
  else
    {{
      <aside class="section-sidebar" aria-label={{title}}>
        <div class="section-sidebar__title">{{title}}</div>
        <nav>
          <ol>{{found.map <| itemHtml pagePath}}</ol>
        </nav>
      </aside>
    }}

end Blog.SectionSidebar

namespace Blog.ReadingStats

def wordsPerMinute : Nat := 225

structure Stats where
  words : Nat
  minutes : Nat

def countWords (text : String) : Nat :=
  let (_, words) := text.foldl (fun (inWord, words) char =>
    if char.isWhitespace then
      (false, words)
    else if inWord then
      (true, words)
    else
      (true, words + 1)) (false, 0)
  words

def minutesFor (words : Nat) : Nat :=
  if words == 0 then 0 else max 1 ((words + wordsPerMinute - 1) / wordsPerMinute)

def fromText (text : String) : Stats :=
  let words := countWords (normalizeSpaces text)
  { words, minutes := minutesFor words }

def fromPost (post : BlogPost) : Stats :=
  fromText <| Text.partText false post.contents

def fromHtml (html : Html) : Stats :=
  fromText <| htmlText "\n" (fun _ attrs => classContains "footnote" attrs) html

def minuteLabel (minutes : Nat) : String :=
  if minutes == 1 then "1 min read" else s!"{minutes} min read"

def render (stats : Stats) : Html :=
  if stats.words == 0 then
    Html.empty
  else
    {{<div class="reading-stats">{{minuteLabel stats.minutes}}</div>}}

end Blog.ReadingStats

namespace Blog.Anchors

open Std

-- Don't use Multi.Slug as it has poor non-English suppoty
abbrev M := StateM (HashSet String)

def anchorBase (title : String) : String :=
  normalizeSpaces title |>.replace " " "-"

partial def uniqueId (used : HashSet String) (base : String) : String :=
  if !used.contains base then
    base
  else
    go 2
where
  go (n : Nat) :=
    let candidate := s!"{base}-{n}"
    if used.contains candidate then
      go (n + 1)
    else
      candidate

partial def collectIds (part : Part Post) : M Unit := do
  if let some htmlId := part.metadata >>= (·.htmlId) then
    modify (·.insert htmlId)
  part.subParts.forM collectIds

def withId (fallback : Post.Meta) (part : Part Post) (htmlId : String) : Post.Meta :=
  match part.metadata with
  | some md => { md with htmlId := some htmlId }
  | none => { fallback with htmlId := some htmlId }

partial def addPartIds
    (fallback : Post.Meta)
    (part : Part Post) : M (Part Post) := do
  let part ←
    match part.metadata >>= (·.htmlId) with
    | some _ => pure part
    | none =>
      let base := anchorBase part.titleString
      let id := uniqueId (← get) base
      modify (·.insert id)
      pure {
        part with
        metadata := some (withId fallback part id)
      }
  let subParts ← part.subParts.mapM (addPartIds fallback)
  pure { part with subParts }

def addPostIds (post : BlogPost) : BlogPost :=
  match post.contents.metadata with
  | none => post
  | some rootMetadata =>
    let used := (collectIds post.contents).run {} |>.2
    let subParts := post.contents.subParts.mapM (addPartIds rootMetadata) |>.run' used
    { post with contents := { post.contents with subParts } }

def addDirIds : Dir → Dir
  | .page name id txt contents => .page name id txt (contents.map addDirIds)
  | .blog name id txt posts => .blog name id txt (posts.map addPostIds)
  | .static name files => .static name files

def addSectionAnchors : Site → Site
  | .page id txt contents => .page id txt (contents.map addDirIds)
  | .blog id txt posts => .blog id txt (posts.map addPostIds)

end Blog.Anchors
