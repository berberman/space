import VersoBlog
import Blog.Categories
import Blog.Meta
open Verso Genre Blog

#doc (Post) "Hello Verso" =>

%%%
authors := ["berberman"]
date := {year := 2026, month := 2, day := 14}
categories := [Category.blog]
%%%

```leanInit empty
```

It's been 6 years since I first built this site using [Hakyll](https://github.com/jaspervdj/hakyll)—though I barely posted anything.
Recently, I decided to migrate to [Verso](https://github.com/leanprover/verso),
a new static site generator written in Lean. It allows me to write posts directly in Lean and seamlessly integrating code snippets:

```lean empty
theorem Eq.uip {α : Sort u}
    (x y : α) (h₁ h₂ : x = y) : h₁ = h₂ := by
  rfl
```

```lean empty (name := succ)
#check Nat.succ
```

```leanOutput succ
Nat.succ (n : Nat) : Nat
```

I adapted this site from [verso-templates](https://github.com/leanprover/verso-templates/tree/main/blog),
and with some fine-tuning, I've got [Prism.js](https://prismjs.com/) working for non-Lean languages:

```haskell
main :: IO ()
main = putStrLn "Hello, World!"
```

and [typst](https://typst.app/) with [mathyml](https://codeberg.org/akida/mathyml) for typesetting math {typst}[$`f(x)`] formulas:

{typst}[$$`(1/ sqrt(2 pi)) e^(-x^2 / 2) quad mat(1, 2; 3, 4)`]

{typst}[$$`pi(x)`]

{typst}[$$`lr(a/b\]) = a = lr(\{a/b))`]

{typst}[$$`mat(1, 0, 1; 0, 1, 2; augment: #2)`]

{typst}[$$`mat(augment: #(vline: (-2, 2, 1), hline: (1, -1), stroke: 0.1em),
  11, 12, 13, 14, 15;
  21, 22, 23, 24, 25;
  31, 32, 33, 34, 35;
  41, 42, 43, 44, 45;)`]

I hope to use this refresh to share more updates and thoughts on programming. Stay tuned!
