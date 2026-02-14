import VersoBlog
import Blog.Categories
open Verso Genre Blog

#doc (Post) "Hell Verso" =>

%%%
authors := ["berberman"]
date := {year := 2026, month := 2, day := 14}
categories := [lean]
%%%

```leanInit empty
```

It's been 6 years since I first built this site using [Hakyll](https://github.com/jaspervdj/hakyll)—though I barely posted anything.
Recently, I decided to migrate to [Verso](https://github.com/leanprover/verso),
a new static site generator written in Lean. It allows me to write posts directly in Lean and seamlessly integrating code snippets:

```Verso.Genre.Blog.lean empty
theorem Eq.uip {α : Sort u}
    (x y : α) (h₁ h₂ : x = y) : h₁ = h₂ := by
  rfl
```

```Verso.Genre.Blog.lean empty (name := succ)
#check Nat.succ
```

```leanOutput succ
Nat.succ (n : Nat) : Nat
```

I adapted this site from [verso-templates](https://github.com/leanprover/verso-templates/tree/main/blog),
and am working on:
* syntax highlighting for non-Lean languages
* [typst](https://typst.app/) integration for elegant math formulas
* RSS feed

I hope to use this refresh to share more updates and thoughts on programming. Stay tuned!
