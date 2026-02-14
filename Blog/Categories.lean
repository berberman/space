
import VersoBlog

open Verso.Genre.Blog.Post

namespace Blog

def lean : Category where
  name := "Lean"
  slug := "lean"

def haskell : Category where
  name := "Haskell"
  slug := "haskell"

def typeTheory : Category where
  name := "Type Theory"
  slug := "type-theory"

def other : Category where
  name := "Other"
  slug := "other"
