import Blog.Site.Basic
import Blog.Site.Render
import Blog.Atom
import Blog.SEO

open Verso Genre Blog Site Syntax Doc Output Html SiteConfig Template Theme

namespace Blog.Theme

def isScribblePath (path : Multi.Path) : Bool :=
  match path.toList with
  | "scribbles" :: _ => true
  | _ => false

def dateForPath (path : Multi.Path) (date : Date) : String :=
  if isScribblePath path then
    s!"{date.year}年{date.month}月{date.day}日"
  else
    date.toReadable

def mainClassForPath (path : Multi.Path) : String :=
  match path.toList with
  | [] => "page page--home"
  | ["about"] => "page page--about"
  | ["academic"] => "page page--academic"
  | "scribbles" :: _ => "page page--scribbles"
  | _ => "page"

def theme : Theme := { Theme.default with
  archiveEntryTemplate := do
    let path ← currentPath
    let isScribble := isScribblePath path
    let readMoreLabel := if isScribble then "继续阅读" else "Read more"
    let post : BlogPost ← param "post"
    let summary ← param "summary"
    let target ← if let some p := (← param? "path") then
        pure <| p ++ "/" ++ (← post.postName')
      else post.postName'
    let catAddr ← do
      if let some p := (← param? "path") then
        pure <| fun slug => p ++ "/" ++ slug
      else pure <| fun slug => slug
    -- let stats := ReadingStats.fromPost post

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
                {{dateForPath path (md : Post.PartMetadata).date}}
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
              -- Don't show ReadingStats for scribbles
              -- Disabled for now. Need to skip math code
              -- {{if isScribble then Html.empty else ReadingStats.render stats}}
            </div>
           }}
          }}
        {{Footnotes.runSummary target summary}}
        <div class="archive-action-row">
          <a href={{target}} class="read-more">{{readMoreLabel}}</a>
        </div>
      </li>
    }}]
  postTemplate := do
    let path ← currentPath
    let isScribble := isScribblePath path
    let giscusLang := if isScribble then "zh-CN" else "en"
    let pagePath := routePath path.toList
    let catAddr ← do
      if let some p := (← param? "path") then
        pure <| fun slug => p ++ "/" ++ slug
      else pure <| fun slug => slug
    let rawContent : Html ← param "content"
    let stats := Blog.ReadingStats.fromHtml rawContent
    let metadata := match (← param? "metadata") with
         | none => Html.empty
         | some md => {{
          <div class="metadata">
            <div class="date">
              {{dateForPath path (md : Post.PartMetadata).date}}
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
            -- Similarly
            -- {{if isScribble then Html.empty else Blog.ReadingStats.render stats}}
          </div>
         }}
    let mut content := rawContent
    content := Footnotes.run pagePath content
    content := HeadingLinks.run pagePath content
    let sidebarTitle := if isScribble then "本文目录" else "On this page"
    let sidebar := SectionSidebar.render pagePath sidebarTitle content
    pure {{
      <h1>{{← param "title"}}</h1>
      {{ metadata }}
      {{ sidebar }}
      {{ content }}
      <section class="comments" aria-label="Comments">
        <script>
          {{ Html.text false s!"var s=document.createElement('script'),t=document.documentElement.dataset.theme,l='{absoluteUrl "/static/giscus.css"}',d='{absoluteUrl "/static/giscus-dark.css"}';s.src='https://giscus.app/client.js';s.setAttribute('data-repo','berberman/space');s.setAttribute('data-repo-id','MDEwOlJlcG9zaXRvcnkzMDc2NTQ0Njg=');s.setAttribute('data-category','Announcements');s.setAttribute('data-category-id','DIC_kwDOElZvRM4C__Y6');s.setAttribute('data-mapping','title');s.setAttribute('data-strict','0');s.setAttribute('data-reactions-enabled','1');s.setAttribute('data-emit-metadata','0');s.setAttribute('data-input-position','bottom');s.setAttribute('data-theme-light',l);s.setAttribute('data-theme-dark',d);s.setAttribute('data-theme',t==='dark'?d:l);s.setAttribute('data-lang','{giscusLang}');s.setAttribute('crossorigin','anonymous');s.async=true;document.currentScript.parentElement.appendChild(s)" }}
        </script>
      </section>
    }}
  primaryTemplate := do
    let path ← currentPath
    let isScribble := isScribblePath path
    let postsLabel := if isScribble then "小碎片" else "Posts"
    let categoriesLabel := if isScribble then "分类" else "Categories"
    let htmlLang := if isScribble then "zh-CN" else "en"
    let postList :=
      match (← param? "posts") with
      | none => Html.empty
      | some html => {{
        <section class="archive-posts">
          <div class="archive-section-label">{{postsLabel}}</div>
          {{html}}
        </section>
      }}
    let catList :=
      match (← param? (α := Post.Categories) "categories") with
      | none => Html.empty
      | some ⟨cats⟩ => {{
        <section class="category-directory archive-categories">
          <div class="archive-section-label">{{categoriesLabel}}</div>
          <ul>
            {{ cats.map fun (target, cat) =>
              {{<li><a href={{target}}>{{Post.Category.name cat}}</a></li>}}
            }}
          </ul>
        </section>
      }}
    let title ← param (α := String) "title"
    let rawPageContent ← param (α := Html) "content"
    let ogImage? := Blog.SEO.imageFromHtml rawPageContent
    let content := Blog.SEO.stripImageMarkers rawPageContent
    let description := Blog.SEO.descriptionFromHtml content
    let canonical := canonicalUrl path
    let mainClass := mainClassForPath path
    let metadata? ← param? (α := Post.PartMetadata) "metadata"
    let seoTags := Blog.SEO.metaTags title description canonical metadata? ogImage?
    return {{
      <html lang={{htmlLang}}>
        <head>
          <meta charset="utf-8"/>
          <meta name="viewport" content="width=device-width, initial-scale=1"/>
          <title>{{ title }}</title>
          <script>
            {{ Html.text false "try{var t=localStorage.getItem('space-theme');if(t!=='light'&&t!=='dark'){t=matchMedia('(prefers-color-scheme: dark)').matches?'dark':'light'}document.documentElement.dataset.theme=t}catch(e){document.documentElement.dataset.theme=matchMedia('(prefers-color-scheme: dark)').matches?'dark':'light'}" }}
          </script>
          {{ seoTags }}
          <link rel="stylesheet" href="https://fred-wang.github.io/MathFonts/NewComputerModern/mathfonts.css"/>
          <link rel="icon" type="image/x-icon" href={{faviconPath}}/>
          <link rel="alternate" type="application/atom+xml" title={{siteTitle}} href={{absoluteUrl atomFeedPath}}/>
          {{← builtinHeader }}
          <link rel="stylesheet" href="/static/style.css"/>
          <link rel="stylesheet" href="/static/academic.css"/>
          <link href="/static/prism.css" rel="stylesheet" />
        </head>
        <body class="md2-app-shell">
          <header class="site-header md2-app-bar">
            <button class="md2-menu-button" type="button" aria-label="Open navigation" aria-controls="site-drawer" aria-expanded="false">
              <svg viewBox="0 0 24 24" aria-hidden="true" focusable="false">
                <path d="M3 18h18v-2H3v2zm0-5h18v-2H3v2zm0-7v2h18V6H3z"/>
              </svg>
            </button>
            <a class="logo" href="/" aria-label="Space home">"Space"</a>
            <div class="md2-desktop-nav" aria-label="Primary navigation">
              {{ ← topNav }}
            </div>
            <button class="md2-theme-toggle" type="button" aria-label="Switch to dark theme" aria-pressed="false">
              <svg class="md2-theme-toggle__icon md2-theme-toggle__icon--dark" viewBox="0 0 24 24" aria-hidden="true" focusable="false">
                <path d="M9.37 5.51A7 7 0 0 0 18.5 14.63 7 7 0 1 1 9.37 5.51Z"/>
              </svg>
              <svg class="md2-theme-toggle__icon md2-theme-toggle__icon--light" viewBox="0 0 24 24" aria-hidden="true" focusable="false">
                <path d="M12 4V2m0 20v-2m8-8h2M2 12h2m13.66-5.66 1.41-1.41M4.93 19.07l1.41-1.41m0-11.32L4.93 4.93m14.14 14.14-1.41-1.41M12 17a5 5 0 1 0 0-10 5 5 0 0 0 0 10Z"/>
              </svg>
            </button>
          </header>
          <aside id="site-drawer" class="md2-drawer" aria-label="Site navigation">
            {{ ← topNav }}
          </aside>
          <button class="md2-drawer-scrim" type="button" aria-label="Close navigation"></button>
          <div class="md2-app-content">
            <main class={{mainClass}}>
              {{ content }}
              {{ postList }}
              {{ catList }}
            </main>
            <footer>
               "© 2020-2026 ❤"
              <a href="https://github.com/berberman">"berberman"</a>
              <a class="feed-link" href={{atomFeedPath}} aria-label="Atom feed"><img src={{feedIconPath}} alt="" aria-hidden="true"/>"Atom"</a>
            </footer>
          </div>
          <script src="/static/prism.js"></script>
          <script src="/static/section-sidebar.js"></script>
          <script src="/static/reading-ui.js"></script>
        </body>
      </html>
    }}
  }
  |>.override #[] {
    template := do
      return {{<div class="frontpage">{{← param "content"}}</div>}},
    params := id
  }

end Blog.Theme
