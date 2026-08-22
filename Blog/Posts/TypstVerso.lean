import VersoBlog
import Blog.Categories
import Blog.Site.Extensions
open Verso Genre Blog

#doc (Post) "Embedding Typst in Verso" =>

%%%
authors := ["berberman"]
date := {year := 2026, month := 8, day := 22}
categories := [Category.meta]
%%%

Now it comes to my third meta blog post — a blog post about the techniques used to build this site.

# The First Attempt: MathML

Before Typst actually supports HTML output with MathML in version [0.15](https://typst.app/blog/2026/typst-0.15/),
I was using [`akida/mathyml`](https://codeberg.org/akida/mathyml), a third-party Typst library that does something like
along the lines of converting formuals to MathML by inspecting math expressions and  constructing HTML elements manually.

This sounds sketchy but it actually worked. MathML is native to the web, and browsers know how to render it.

The very first architecture I had, even before {page_link Blog.Posts.HelloVerso}[HelloVerso] was post,
was to call a Python script whenever Verso encoutered `{typst}...` syntax.
The script would create a small Typst document, include the `mathyml` setup followed by the formula,
run the Typst app, and return the resulting HTML during elaboration.

At frist glance, this seemd rather elegant. With verso's incrementality, the results could be cached,
and errors could be reported directly at the source syntax, which was quite tempting.

However, before I even ran into the visual problems, the bad performance showed up.

Imagine that there are 100 mathmatical formulas in a single document — which is entirely possible.
Verso would need to create subprocess to run Python, which would then run Typst, *100 times*.
There was a noticeable lag during document elaboration.

Clearly, this wasn't a plausible design.

# Batching the Rendering

I reworked the system around a "batching" workflow.

Instead of running Typst during document elaboration, Verso would directly emit

```
<span class = "typst-inline"> ... </span>
```

or

```
<div class="typst-block"> ... </div>
```

with the raw Typst source embedded inside.

After Verso had finished building the entire site, another Python script would post-process the generated HTML.
It would find all these elements, synthesize a large Typst document containing all the formulas,
render that document in one go, and then extract the corresponding HTML snippets.

This sounds even worse, but it improved the performance by quite a lot.
And for a while, I was quite happy with this hacky implementation.

Then I got distracted by the rendered MathML.

# The Endless Battle with MathML

The formulas looked awkward.

Even after configuring math fonts and CSS, there were still mysterious layout problems,
baseline alighment, spacing between containers such as opening and closing braces,
inconsistent sizing between inline and display math, and so on.

I felt like an endless sequence of adjustments. Fix one thing, and something else would look slightly wrong.
I even had a heuristic in the Python script that would disable stretchiness on fences unless they contain 'tall' math elements.

The fundamental problem was that Typst already knew how the formula was supposed to look,
but I was throwing that layout information away and asking the browser to reconstruct it from MathML.

Eventually, I decided that I had had enough.

# Giving Up on MathML


I decided to abandon MathML and switch to SVG.

I've seen people use Typst's SVG target to generate their entire websites.
That doesn't make for a particularly good browsing experience: search engines can't index the content properly,
font sizes and layout are fixed, and you can't select the text.

But that isn't a problem for my use case.
I'm not turning the entire website into SVG.
I'm just replacing mathematical formulas with SVGs, while leaving everything else as ordinary HTML.

And this actually gives me exactly what I want: let Typst handle the layout, and let the browser display the result.

Of course, I could have kept the old architecture.
I could have generated HTML stubs, had a Python script call Typst to generate SVGs,
and then filled the SVGs back into the HTML. The only difference would have been using SVG instead of MathML.

But at that point I figured it was time to switch to a more modern — or perhaps just more proper — architecture.

# Typst as Part of Elaboration

So I wrote a small Rust program (with help from LLMs) depending on the [`typst`](https://crates.io/crates/typst) crate that acts as an RPC server.

Each Lean elaboration process starts one of these servers.
When Verso encounters `{typst}...`, Lean sends a JSON request containing the Typst source code to the server.

The Rust server receives the source, invokes the Typst library,
and returns the generated SVG along with any diagnostics.
It also maintains its own cache, so the same Typst source doesn't need to be rendered repeatedly.

Verso then receives the response and directly embeds the SVG into the generated HTML.
More importantly, it takes the source spans of any Typst diagnostics and turns them back into Lean diagnostics,
publishing them into Lean's info tree.

This gives a spectacular editing experience.
If something goes wrong in the Typst code, errors and warnings immediately pop up in the editor,
just like they would for Lean code.

And this is where the project became more interesting —
Typst is no longer something that runs after the site has been built
From Lean's point of view, it has become part of the document language.

# A Rust Program That Is a Lean Dependency

There is another amusing consequence of this architecture.

The Rust program is treated as part of the Lean source code in the project, with `cargo` serving as its build recipe.

This means that if I change the Rust code, Lean LSP notices:

> Imports are out of date and must be rebuilt; use the "Restart File" command in your editor.

And rebuilding the dependency actually rebuilds the Rust program.

This makes the Rust/Lean boundary feel surprisingly seamless.
The RPC server is an external process at runtime, but from the project's build-system perspective,
it is simply another dependency.

# Three Layers of Caching

The whole thing is also _blazingly fast_ (just kidding), thanks to several layers of caching.
1. Verso, or Lean's incrementality. Elaboration results are stored in `.olean` files and can be reused when the source has not changed.
2. The explicit cache in the Rust server. A given Typst source maps to a particular rendering, so the resulting SVG can be cached.
3. Typst's own internal incrementality. Typst itself has its own mechanisms for avoiding unnecessary work.

Together, these make the cost of embedding Typst much less noticeable than the original approach of spawning Typst once per formula.

# SVG Comes to Rescue

And, finally, with SVG, the layout headache is gone.

With a little bit of scaling, the rendered result just works.
Typst decides how the formula should look, and the browser simply displays the resulting geometry.
I no longer have to worry about whether a browser will interpret some MathML spacing or baseline rule in the way I intended.

Here are some examples taken from the [Typst Example Book](https://sitandr.github.io/typst-examples-book/book/basics/math/index.html)
and some Typst libraries:

Let {typst}`a`, {typst}`b`, and {typst}`c` be the side lengths of right-angled triangle.
Then, we know that:

{typst +display}`a^2 + b^2 = c^2`

Prove by induction:

{typst +display}`sum_(k = 1)^n k = (n(n+1)/2)`

{typst +display}`
#let ax = rule.with(name: [ax])
#let and-el = rule.with(name: $and_e^ell$)
#let and-er = rule.with(name: $and_e^r$)
#let impl-i = rule.with(name: $scripts(->)_i$)
#let impl-e = rule.with(name: $scripts(->)_e$)
#let not-i = rule.with(name: $not_i$)
#let not-e = rule.with(name: $not_e$)

#prooftree(
  impl-i(
    not-i(
      not-e(
        impl-e(
          ax($Gamma tack p -> q$),
          and-el(
            ax($Gamma tack p and not q$),
            $Gamma tack p$,
          ),
          $Gamma tack q$,
        ),
        and-er(
          ax($Gamma tack p and not q$),
          $Gamma tack not q$,
        ),
        $ underbrace(p -> q\, p and not q, Gamma) tack bot $,
      ),
      $p -> q tack  not (p and not q)$,
    ),
    $tack (p -> q) -> not (p and not q)$,
  )
)
`

{typst +display}`
#align(center)[#commutative-diagram(
  node((0, 0), $X$),
  node((0, 1), $Y$),
  node((1, 0), $X \/ "ker"(f)$, "quot"),
  arr($X$, $Y$, $f$),
  arr("quot", (0, 1), $tilde(f)$, label-pos: right, "dashed", "inj"),
  arr($X$, "quot", $pi$),
)]
`
