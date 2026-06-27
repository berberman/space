import VersoBlog

import Blog

open Verso Genre Blog Site Syntax Doc Output Html

def Verso.Genre.Blog.Date.toReadable (date: Date) : String :=
  let m := match date.month with
    | 1 => "January"
    | 2 => "February"
    | 3 => "March"
    | 4 => "April"
    | 5 => "May"
    | 6 => "June"
    | 7 => "July"
    | 8 => "August"
    | 9 => "September"
    | 10 => "October"
    | 11 => "November"
    | 12 => "December"
    | _ => "Unknown"
  s!"{m} {date.day}, {date.year}"

open Output Html Template Theme

namespace HeadingLinks

/-- Find the value of an HTML attribute. -/
def attr? (name : String) (attrs : Array (String × String)) : Option String := do
  let attr ← attrs.find? (·.1 == name)
  return attr.2

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

end HeadingLinks

namespace Footnotes

abbrev Note := Html

/-- Notes already encountered in this post. -/
abbrev M := StateM (Array Note)

def isFootnote (name : String) (attrs : Array (String × String)) : Bool :=
  name == "details" && HeadingLinks.attr? "class" attrs == some "footnote"

/-- Remove Verso's inline footnote summary from the collected note body. -/
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

/-- Replace inline footnotes with references and append a footnote list -/
def run (pagePath : String) (content : Html) : Html :=
  let (content, notes) := (collect pagePath content).run #[]
  if notes.isEmpty then content else content ++ renderSection pagePath notes

/-- Replace summary footnotes with links to the full post's footnotes. -/
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

end Footnotes

def theme : Theme := { Theme.default with
  archiveEntryTemplate : Template := do
    let post : BlogPost ← param "post"
    let summary ← param "summary"
    let target ← if let some p := (← param? "path") then
        pure <| p ++ "/" ++ (← post.postName')
      else post.postName'
    let catAddr ← do
      if let some p := (← param? "path") then
        pure <| fun slug => p ++ "/" ++ slug
      else pure <| fun slug => slug

    return #[{{
      <li>
        <a href={{target}} class="title">
          <span class="name">{{post.contents.titleString}}</span>
        </a>
        {{ match post.contents.metadata with
           | none => Html.empty
           | some md => {{
            <div class="metadata">
              <div class="date">
                {{(md : Post.PartMetadata).date.toReadable}}
              </div>
              <div class="authors">
                {{(md : Post.PartMetadata).authors.map ({{<span class="author">{{Html.text true ·}}</span>}}) |>.toArray}}
              </div>
              {{if md.categories.isEmpty then Html.empty
                else {{
                  <ul class="categories">
                    {{md.categories.toArray.map (fun (cat : Post.Category) => {{<li><a href=s!"{catAddr cat.slug}">{{cat.name}}</a></li>}})}}
                  </ul>
                }}
              }}
            </div>
           }}
         }}
        {{Footnotes.runSummary target summary}}
        <a href={{target}} class="read-more">"Read more"</a>
      </li>
    }}]
  postTemplate := do
    let catAddr ← do
      if let some p := (← param? "path") then
        pure <| fun slug => p ++ "/" ++ slug
      else pure <| fun slug => slug
    let pagePath := "/" ++ "/".intercalate (← currentPath).toList ++ "/"
    let metadata := match (← param? "metadata") with
         | none => Html.empty
         | some md => {{
          <div class="metadata">
            <div class="date">
              {{(md : Post.PartMetadata).date.toReadable}}
            </div>
            <div class="authors">
              {{(md : Post.PartMetadata).authors.map ({{<span class="author">{{Html.text true ·}}</span>}}) |>.toArray}}
            </div>
            {{if md.categories.isEmpty then Html.empty
              else {{
                <ul class="categories">
                  {{md.categories.toArray.map (fun (cat : Post.Category) => {{<li><a href=s!"{catAddr cat.slug}">{{cat.name}}</a></li>}})}}
                </ul>
              }}
            }}
          </div>
         }}
    pure {{
      <h1>{{← param "title"}}</h1>
      {{ metadata }}
      {{HeadingLinks.run pagePath (Footnotes.run pagePath (← param "content"))}}
      <section class="comments" aria-label="Comments">
        <script src="https://giscus.app/client.js"
                data-repo="berberman/space"
                data-repo-id="MDEwOlJlcG9zaXRvcnkzMDc2NTQ0Njg="
                data-category="Announcements"
                data-category-id="DIC_kwDOElZvRM4C__Y6"
                data-mapping="title"
                data-strict="0"
                data-reactions-enabled="1"
                data-emit-metadata="0"
                data-input-position="bottom"
                data-theme="light"
                data-lang="en"
                crossorigin="anonymous"
                async>
        </script>
      </section>
    }}
  primaryTemplate := do
    let postList :=
      match (← param? "posts") with
      | none => Html.empty
      | some html => {{
          <section class="archive-posts">
            <div class="archive-section-label">"Posts"</div>
            {{html}}
          </section>
        }}
    let catList :=
      match (← param? (α := Post.Categories) "categories") with
      | none => Html.empty
      | some ⟨cats⟩ => {{
          <section class="category-directory archive-categories">
            <div class="archive-section-label">"Categories"</div>
            <ul>
            {{ cats.map fun (target, cat) =>
              {{<li><a href={{target}}>{{Post.Category.name cat}}</a></li>}}
            }}
            </ul>
          </section>
        }}
    return {{
      <html>
        <head>
          <meta charset="utf-8"/>
          <meta name="viewport" content="width=device-width, initial-scale=1"/>
          <title>{{ (← param (α := String) "title") }}</title>
          <link rel="stylesheet" href="https://fred-wang.github.io/MathFonts/NewComputerModern/mathfonts.css"/>
          <link rel="icon" type="image/x-icon" href="/static/favicon.ico"/>
          {{← builtinHeader }}
          <link rel="stylesheet" href="/static/style.css"/>
          <link href="/static/prism.css" rel="stylesheet" />
        </head>
        <body>
          <header>
            <div class="logo"><a href="/">"Space"</a></div>
            {{ ← topNav }}
          </header>
          <main>
            {{← param "content" }}
            {{ catList }}
            {{ postList }}
          </main>
          <footer>
             "© 2020-2026 ❤"
            <a href="https://github.com/berberman">"berberman"</a>
          </footer>
          <script src="/static/prism.js"></script>
        </body>
      </html>
    }}
  }
  |>.override #[] {
    template := do
      return {{<div class="frontpage">{{← param "content"}}</div>}},
    params := id
  }

namespace Anchors

open Std

/-- We keep track of IDs already used in the current post. -/
abbrev M := StateM (HashSet Multi.Slug)

def idSlug? (metadata : Option Post.Meta) : Option Multi.Slug := do
  let md ← metadata
  let htmlId ← md.htmlId
  return htmlId.sluggify

/-- Collect all existing section IDs -/
partial def collectIds (part : Part Post) : M Unit := do
  if let some htmlId := idSlug? part.metadata then
    modify (·.insert htmlId)
  part.subParts.forM collectIds

/-- Set `htmlId` on a part's metadata -/
def withId (fallback : Post.Meta) (part : Part Post) (htmlId : String) : Post.Meta :=
  match part.metadata with
  | some md => { md with htmlId := some htmlId }
  | none => { fallback with htmlId := some htmlId }

/--
Add IDs to one section subtree.

Each section keeps an explicit `htmlId` if it has one;
otherwise its title is slugified and made unique within the post.
-/
partial def addPartIds (fallback : Post.Meta) (part : Part Post) : M (Part Post) := do
  let part ←
    match part.metadata >>= (·.htmlId) with
    | some _ => pure part
    | none =>
      let slug := Multi.Slug.unique (← get) part.titleString.sluggify
      modify (·.insert slug)
      pure { part with metadata := some (withId fallback part slug.toString) }
  let subParts ← part.subParts.mapM (addPartIds fallback)
  pure { part with subParts }

/--
Add generated section IDs to a blog post.

The root post metadata is not changed because the template renders the post title itself
-/
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

/--
Add stable HTML IDs to every blog post section heading.

We fill `htmlId` on post subparts before VersoBlog renders them.
-/
def addSectionAnchors : Site → Site
  | .page id txt contents => .page id txt (contents.map addDirIds)
  | .blog id txt posts => .blog id txt (posts.map addPostIds)

end Anchors

def blog : Site := site Blog.FrontPage /
  static "static" ← "static_files"
  "about" Blog.About
  "blog" Blog.Posts with
    Blog.Posts.ConvoyPatterns
    Blog.Posts.HEqAndAxiomK
    Blog.Posts.HelloVerso
    Blog.Posts.UploadAndroidLibsToGitHub
    Blog.Posts.ExtsInGHCi
    Blog.Posts.ArchHaskell

def main (options : List String) := do
  let x ← blogMain theme (Anchors.addSectionAnchors blog) (options := options)
  let stdout ← IO.Process.run {
    cmd := "python3",
    args := #["typst/process_math.py", "_site"]
  }
  IO.println stdout
  pure x
