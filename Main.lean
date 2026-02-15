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

open Output Html Template Theme in

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
        {{summary}}
        <a href={{target}} class="read-more">"Read more"</a>
      </li>
    }}]
  postTemplate := do
    let catAddr ← do
      if let some p := (← param? "path") then
        pure <| fun slug => p ++ "/" ++ slug
      else pure <| fun slug => slug
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
      {{← param "content"}}
      <script src="https://utteranc.es/client.js"
        repo="berberman/space"
        issue-term="pathname"
        theme="github-light"
        crossorigin="anonymous" async>
      </script>
    }}
  primaryTemplate := do
    let postList :=
      match (← param? "posts") with
      | none => Html.empty
      | some html => {{ <h2> "Posts" </h2> }} ++ html
    let catList :=
      match (← param? (α := Post.Categories) "categories") with
      | none => Html.empty
      | some ⟨cats⟩ => {{
          <div class="category-directory">
            <h2> "Categories" </h2>
            <ul>
            {{ cats.map fun (target, cat) =>
              {{<li><a href={{target}}>{{Post.Category.name cat}}</a></li>}}
            }}
            </ul>
          </div>
        }}
    return {{
      <html>
        <head>
          <meta charset="utf-8"/>
          <meta name="viewport" content="width=device-width, initial-scale=1"/>
          <title>{{ (← param (α := String) "title") }}</title>
          <link rel="stylesheet" href="/static/style.css"/>
          <link href="/static/prism.css" rel="stylesheet" />
          <link rel="stylesheet" href="https://fred-wang.github.io/MathFonts/NewComputerModern/mathfonts.css"/>
          <link rel="icon" type="image/x-icon" href="/static/favicon.ico"/>
          {{← builtinHeader }}
        </head>
        <body>
          <header>
            <div class="logo"><a href="/">"Space"</a></div>
            <nav>{{ ← topNav }}</nav>
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


def blog : Site := site Blog.FrontPage /
  static "static" ← "static_files"
  "about" Blog.About
  "blog" Blog.Posts with
    Blog.Posts.HelloVerso
    Blog.Posts.UploadAndroidLibsToGitHub
    Blog.Posts.ExtsInGHCi
    Blog.Posts.ArchHaskell

def main := blogMain theme blog
