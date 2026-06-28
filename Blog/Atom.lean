import Blog.Html
import Blog.SiteConfig

open Verso Genre Blog Site Doc
open Blog.Html Blog.SiteConfig

namespace Blog.Atom

abbrev M := ReaderT Config IO

def showDrafts : M Bool := read <&> (·.showDrafts)

structure Entry where
  title : String
  url : String
  updated : String
  authors : List String
  summary : String

partial def inlineText : Inline Post → String
  | .text str => str
  | .code str => str
  | .emph content
  | .bold content
  | .link content ..
  | .concat content
  | .footnote _ content
  | .other _ content => textList content.toList
  | .math _ str => str
  | .linebreak _ => "\n"
  | .image alt _ => alt
where
  textList (content : List (Inline Post)) : String :=
    "".intercalate (content.map inlineText)

partial def blockText : Block Post → String
  | .para content => "".intercalate (content.toList.map inlineText)
  | .ul items
  | .ol _ items =>
    "\n".intercalate <| items.toList.map fun item =>
      "\n".intercalate (item.contents.toList.map blockText)
  | .dl items =>
    "\n".intercalate <| items.toList.map fun
      | ⟨term, contents⟩ =>
        "".intercalate (term.toList.map inlineText) ++ " " ++
        "\n".intercalate (contents.toList.map blockText)
  | .blockquote items
  | .concat items
  | .other _ items => "\n".intercalate (items.toList.map blockText)
  | .code str => str

mutual
  partial def partSummary (part : Part Post) : String :=
    let content := "\n\n".intercalate (part.content.toList.map blockText)
    if content.isEmpty then partsSummary part.subParts else content

  partial def partsSummary (parts : Array (Part Post)) : String := Id.run do
    for part in parts do
      let summary := partSummary part
      if !summary.isEmpty then
        return summary
    return ""
end

def postSummary (post : BlogPost) : String :=
  let summary := "\n\n".intercalate (post.summary.toList.map blockText)
  if summary.isEmpty then partSummary post.contents else summary

def entryXml (entry : Entry) : String :=
  let authors :=
    entry.authors.map (fun author =>
      s!"    <author><name>{xmlEscape author}</name></author>\n")
    |> String.join
  s!"  <entry>\n" ++
  s!"    <title>{xmlEscape entry.title}</title>\n" ++
  s!"    <link href=\"{xmlEscape entry.url}\"/>\n" ++
  s!"    <id>{xmlEscape entry.url}</id>\n" ++
  s!"    <updated>{entry.updated}</updated>\n" ++
  authors ++
  s!"    <summary type=\"text\">{xmlEscape entry.summary}</summary>\n" ++
  s!"  </entry>\n"

def feedUpdated : List Entry → String
  | [] => "1970-01-01T00:00:00Z"
  | entry :: _ => entry.updated

def feedXml (entries : List Entry) : String :=
  let siteUrl := absoluteUrl "/"
  let feedUrl := absoluteUrl atomFeedPath
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" ++
  "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n" ++
  s!"  <title>{xmlEscape siteTitle}</title>\n" ++
  s!"  <link href=\"{xmlEscape siteUrl}\"/>\n" ++
  s!"  <link rel=\"self\" href=\"{xmlEscape feedUrl}\"/>\n" ++
  s!"  <id>{xmlEscape siteUrl}</id>\n" ++
  s!"  <icon>{xmlEscape <| absoluteUrl feedIconPath}</icon>\n" ++
  s!"  <logo>{xmlEscape <| absoluteUrl feedIconPath}</logo>\n" ++
  s!"  <updated>{feedUpdated entries}</updated>\n" ++
  s!"  <author><name>{xmlEscape siteAuthor}</name></author>\n" ++
  String.join (entries.map entryXml) ++
  "</feed>\n"

def postEntry (pathToBlog : String) (post : BlogPost) : M (Option Entry) := do
  let some metadata := post.contents.metadata
    | return none
  if metadata.draft && !(← showDrafts) then
    return none
  let slug := (← read).postName metadata.date post.contents.titleString
  let path := s!"/{pathToBlog}/{slug}/"
  return some {
    title := post.contents.titleString,
    url := absoluteUrl path,
    updated := atomTimestamp metadata.date,
    authors := metadata.authors,
    summary := postSummary post
  }

partial def collectDir (path : List String) : Dir → M (List Entry)
  | .page name _ _ contents => do
    let nested ← contents.toList.mapM (collectDir (path ++ [name]))
    return nested.flatten
  | .blog name _ _ posts => do
    let pathToBlog := "/".intercalate (path ++ [name])
    let entries ← posts.toList.filterMapM (postEntry pathToBlog)
    return entries
  | .static .. => return []

def collectSite : Site → M (List Entry)
  | .page _ _ contents => do
    let nested ← contents.toList.mapM (collectDir [])
    return nested.flatten
  | .blog _ _ posts => posts.toList.filterMapM (postEntry "")

def writeFeed (site : Site) (options : List String) : IO Unit := do
  let cfg ← parseOptions options
  let entries ← collectSite site |>.run cfg
  IO.FS.writeFile (cfg.destination.join "atom.xml") (feedXml entries)

end Blog.Atom
