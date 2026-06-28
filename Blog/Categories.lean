
import VersoBlog

open Verso.Genre.Blog.Post

namespace Blog.Category

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

def «meta» : Category where
  name := "Meta"
  slug := "meta"

def nix : Category where
  name := "Nix"
  slug := "nix"
