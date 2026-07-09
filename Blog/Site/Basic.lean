import VersoBlog

open Verso Genre Blog Site Doc Output Html

namespace Blog.Html

def xmlEscape (text : String) : String := Id.run do
  let mut out := ""
  for char in text.toList do
    out := out ++ match char with
      | '&' => "&amp;"
      | '<' => "&lt;"
      | '>' => "&gt;"
      | '"' => "&quot;"
      | '\'' => "&apos;"
      | c => c.toString
  out

def attr? (name : String) (attrs : Array (String × String)) : Option String := do
  let attr ← attrs.find? (·.1 == name)
  return attr.2

def classContains (needle : String) (attrs : Array (String × String)) : Bool :=
  match attr? "class" attrs with
  | none => false
  | some classes => classes.splitOn " " |>.contains needle

partial def htmlText (separator : String) (skip : String → Array (String × String) → Bool) : Html → String
  | .text _ str => str
  | .seq contents => separator.intercalate (contents.toList.map (htmlText separator skip))
  | .tag name attrs contents =>
    if skip name attrs then "" else htmlText separator skip contents

def normalizeSpaces (text : String) : String :=
  let (_, out) := text.foldl (fun (pendingSpace, out) char =>
    if char.isWhitespace then
      (true, out)
    else if pendingSpace && !out.isEmpty then
      (false, out.push ' ' |>.push char)
    else
      (false, out.push char)) (false, "")
  out

end Blog.Html

namespace Blog.SiteConfig

def siteTitle : String := "Space"

def siteAuthor : String := "berberman"

def siteBaseUrl : String := "https://space.torus.icu"

def atomFeedPath : String := "/atom.xml"

def faviconPath : String := "/static/favicon.ico"

def feedIconPath : String := "/static/rss.svg"

/- We don't record timestamp on each post, so just use 12 AM -/
def atomTimestamp (date : Date) : String := s!"{date.toIso8601String}T00:00:00Z"

def Verso.Genre.Blog.Date.toReadable (date : Date) : String :=
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

def absoluteUrl (path : String) : String := s!"{siteBaseUrl}{path}"

def routePath : List String → String
  | [] => "/"
  | steps => "/" ++ "/".intercalate steps ++ "/"

def canonicalUrl (path : Multi.Path) : String := absoluteUrl <| routePath path.toList

def parseOptions (options : List String) : IO Config := go {} options
  where
    go (cfg : Config) : List String → IO Config
      | "--output" :: dir :: more => go { cfg with destination := dir } more
      | "--drafts" :: more => go { cfg with showDrafts := true } more
      | other :: _ => throw <| IO.userError s!"Unknown option {other}"
      | [] => pure cfg

end Blog.SiteConfig

namespace Blog.Text

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

partial def partText (includeTitle : Bool) (part : Part Post) : String :=
  let title := if includeTitle then part.titleString else ""
  let content := "\n\n".intercalate (part.content.toList.map blockText)
  let subParts := "\n\n".intercalate (part.subParts.toList.map (partText true))
  "\n\n".intercalate <| [title, content, subParts].filter (!·.isEmpty)

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

end Blog.Text
