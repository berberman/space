import Blog.Categories
import Blog.Posts
import Blog.About
import Blog.Academic
import Blog.FrontPage
import Blog.Scribbles

open Verso Genre Blog Site Syntax Doc Output Html

def Blog.SiteTree.blog : Site := site Blog.FrontPage /
  static "static" ← "static_files"
  "about" Blog.About
  -- "academic" Blog.Academic
  "blog" Blog.Posts with
    Blog.Posts.MoreVerso
    Blog.Posts.ConvoyPatterns
    Blog.Posts.HEqAndAxiomK
    Blog.Posts.HelloVerso
    Blog.Posts.UploadAndroidLibsToGitHub
    Blog.Posts.ExtsInGHCi
    Blog.Posts.ArchHaskell
  "scribbles" Blog.Scribbles with
    Blog.Scribbles.FirstScribble
