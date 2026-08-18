import VersoBlog

import Blog

open Verso Genre Blog Site Syntax Doc Output Html SiteConfig

def main (options : List String) := do
  let blogSite :=
    SiteTree.blog
    |> CJKLineBreaks.run
    |> Anchors.addSectionAnchors
  let x ← blogMain Theme.theme blogSite (options := options)
  Atom.writeFeed blogSite options
  SEO.writeCrawlerFiles blogSite options
  let stdout ← IO.Process.run {
    cmd := "python3",
    args := #["typst/process_math.py", "_site"]
  }
  IO.println stdout
  pure x
