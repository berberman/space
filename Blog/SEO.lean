import Blog.Site.Basic

open Verso Genre Blog Site Doc Output Html SiteConfig

namespace Blog.SEO

abbrev M := ReaderT Config IO

def showDrafts : M Bool := read <&> (·.showDrafts)

/- TODO: more meaningful description -/
def siteDescription : String := "berberman's space"

structure Image where
  url : String
  alt : String
  width : Option Nat := none
  height : Option Nat := none
  mimeType : Option String := none

/- Wandering Witch: The Journey of Elaina Wandering Witch Vol. 12 Cover
  Contact me for take down.
 -/
def defaultImage : Image := {
  url := "/static/og-elaina.jpg"
  alt := "Space"
  width := some 1200
  height := some 630
  mimeType := some "image/jpeg"
}

def imageUrl (url : String) : String :=
  if url.startsWith "http://" || url.startsWith "https://" then url
  else if url.startsWith "/" then absoluteUrl url
  else absoluteUrl s!"/{url}"

def imageFromAttrs (attrs : Array (String × String)) : Option Image := do
  if !classContains "blog-og-image" attrs then none
  let url ← attr? "data-url" attrs
  let alt ← attr? "data-alt" attrs
  return {
    url
    alt
    width := attr? "data-width" attrs >>= String.toNat?
    height := attr? "data-height" attrs >>= String.toNat?
    mimeType := attr? "data-type" attrs
  }

partial def imageFromHtml : Html → Option Image
  | .text _ _ => none
  | .seq contents => contents.findSome? imageFromHtml
  | .tag _ attrs contents => imageFromAttrs attrs <|> imageFromHtml contents

partial def stripImageMarkers : Html → Html
  | .text escape str => .text escape str
  | .seq contents => .seq <| contents.map stripImageMarkers
  | .tag name attrs contents =>
    if classContains "blog-og-image" attrs then Html.empty
    else .tag name attrs (stripImageMarkers contents)

def themeColor : String := "#f8f5ef"

def skipTag (name : String) (attrs : Array (String × String)) : Bool :=
  ["script", "style", "nav", "aside", "footer", "h1"].contains name ||
    ["metadata", "comments", "section-sidebar", "heading-link", "footnotes", "blog-og-image"].any (classContains · attrs)

def truncate (limit : Nat) (text : String) : String :=
  let chars := text.toList
  if chars.length ≤ limit then
    text
  else
    String.ofList (chars.take limit) ++ "..."

def descriptionFromHtml (content : Html) : String :=
  let description := htmlText " " skipTag content |> normalizeSpaces |> truncate 180
  if description.isEmpty then siteDescription else description

def metaName (name content : String) : Html :=
  .tag "meta" #[("name", name), ("content", content)] Html.empty

def metaProperty (property content : String) : Html :=
  .tag "meta" #[("property", property), ("content", content)] Html.empty

def linkRel (rel href : String) : Html :=
  .tag "link" #[("rel", rel), ("href", href)] Html.empty

def articleMeta (metadata : Post.PartMetadata) : Array Html := Id.run do
  let mut tags := #[]
  tags := tags.push <| metaProperty "article:published_time" (atomTimestamp metadata.date)
  for author in metadata.authors do
    tags := tags.push <| metaProperty "article:author" author
  for category in metadata.categories do
    tags := tags.push <| metaProperty "article:tag" category.name
  return tags

def metaTags (title description canonical : String) (metadata? : Option Post.PartMetadata)
    (image? : Option Image := none) : Html :=
  let image := image?.getD defaultImage
  let ogImage := imageUrl image.url
  let ogType := if metadata?.isSome then "article" else "website"
  let imageTags := #[
    metaProperty "og:image" ogImage,
    metaProperty "og:image:alt" image.alt,
    metaName "twitter:image" ogImage,
    metaName "twitter:image:alt" image.alt
  ] ++
    (image.width.map fun width => #[metaProperty "og:image:width" (toString width)]).getD #[] ++
    (image.height.map fun height => #[metaProperty "og:image:height" (toString height)]).getD #[] ++
    (image.mimeType.map fun mimeType => #[metaProperty "og:image:type" mimeType]).getD #[]
  let base := #[
    metaName "description" description,
    metaName "theme-color" themeColor,
    linkRel "canonical" canonical,
    metaProperty "og:title" title,
    metaProperty "og:description" description,
    metaProperty "og:type" ogType,
    metaProperty "og:url" canonical,
    metaProperty "og:site_name" siteTitle,
    metaName "twitter:card" "summary_large_image",
    metaName "twitter:title" title,
    metaName "twitter:description" description
  ] ++ imageTags
  .seq <| base ++ (metadata?.map articleMeta |>.getD #[])

structure SitemapUrl where
  loc : String
  lastmod? : Option String := none

def sitemapUrlXml (url : SitemapUrl) : String :=
  s!"  <url>\n    <loc>{xmlEscape url.loc}</loc>\n" ++
  (match url.lastmod? with
    | none => ""
    | some lastmod => s!"    <lastmod>{lastmod}</lastmod>\n") ++
  "  </url>\n"

def sitemapXml (urls : List SitemapUrl) : String :=
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" ++
  "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n" ++
  String.join (urls.map sitemapUrlXml) ++
  "</urlset>\n"

def uniqueCategories (posts : Array BlogPost) : Std.HashSet Post.Category := Id.run do
  let mut categories : Std.HashSet Post.Category := {}
  for post in posts do
    if let some metadata := post.contents.metadata then
      categories := categories.union <| Std.HashSet.ofList metadata.categories
  return categories

def postUrl (pathToBlog : String) (post : BlogPost) : M (Option SitemapUrl) := do
  let some metadata := post.contents.metadata
    | return none
  if metadata.draft && !(← showDrafts) then
    return none
  let slug := (← read).postName metadata.date post.contents.titleString
  return some {
    loc := absoluteUrl s!"/{pathToBlog}/{slug}/",
    lastmod? := some metadata.date.toIso8601String
  }

partial def collectDir (path : List String) : Dir → M (List SitemapUrl)
  | .page name _ _ contents => do
    let here := path ++ [name]
    let nested ← contents.toList.mapM (collectDir here)
    return { loc := absoluteUrl (routePath here) } :: nested.flatten
  | .blog name _ _ posts => do
    let here := path ++ [name]
    let pathToBlog := "/".intercalate here
    let postUrls ← posts.toList.filterMapM (postUrl pathToBlog)
    let categoryUrls := uniqueCategories posts |>.toList.map fun category =>
      { loc := absoluteUrl (routePath (here ++ [category.slug])) }
    return { loc := absoluteUrl (routePath here) } :: postUrls ++ categoryUrls
  | .static .. => return []

def collectSite : Site → M (List SitemapUrl)
  | .page _ _ contents => do
    let nested ← contents.toList.mapM (collectDir [])
    return { loc := absoluteUrl "/" } :: nested.flatten
  | .blog _ _ posts => do
    let postUrls ← posts.toList.filterMapM (postUrl "")
    return { loc := absoluteUrl "/" } :: postUrls

def robotsTxt : String :=
  "User-agent: *\n" ++
  "Allow: /\n\n" ++
  s!"Sitemap: {absoluteUrl "/sitemap.xml"}\n"

def writeCrawlerFiles (site : Site) (options : List String) : IO Unit := do
  let cfg ← parseOptions options
  let urls ← collectSite site |>.run cfg
  IO.FS.writeFile (cfg.destination.join "sitemap.xml") (sitemapXml urls)
  IO.FS.writeFile (cfg.destination.join "robots.txt") robotsTxt

end Blog.SEO
