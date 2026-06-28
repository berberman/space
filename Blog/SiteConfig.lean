import VersoBlog

open Verso Genre Blog Site Doc

namespace Blog.SiteConfig

def siteTitle : String := "Space"

def siteAuthor : String := "berberman"

def siteBaseUrl : String := "https://space.torus.icu"

def atomFeedPath : String := "/atom.xml"

/- We don't record timestamp on each post, so just use 12 AM -/
def atomTimestamp (date : Date) : String := s!"{date.toIso8601String}T00:00:00Z"

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
