import Blog.Categories
import Blog.Posts
import Blog.About
import Blog.Academic
import Blog.FrontPage

open Verso Genre Blog Site Syntax Doc Output Html

namespace Blog.SiteTree

def blog : Site := site FrontPage /
  static "static" ← "static_files"
  "about" About
  -- "academic" Academic
  "blog" Posts with
    Posts.MoreVerso
    Posts.ConvoyPatterns
    Posts.HEqAndAxiomK
    Posts.HelloVerso
    Posts.UploadAndroidLibsToGitHub
    Posts.ExtsInGHCi
    Posts.ArchHaskell

end Blog.SiteTree
