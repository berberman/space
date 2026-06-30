import VersoBlog
import Blog.Categories
import Blog.Meta
open Verso Genre Blog

#doc (Post) "More Verso" =>

%%%
authors := ["berberman"]
date := {year := 2026, month := 6, day := 30}
categories := [Category.meta]
%%%

```leanInit e
```

This blog has been around for a while, but every time I visited it, something about the visual design felt off.
The typography was not elegant enough; Lean code blocks had no color scheme; the palette lacked quality; and the font size were not quite right.
Overall, the site did not feel modern, and both UI and UX needed work, even though it retained kind of minimalist style. RSS/Atom feeds were also missing.

So I decided to refine the styling on top of existing foundataion, and add a few interesting features.

# Styling

Thanks to LLMs, I was able to figrue out the styling despite have no frontend skill.
With more fine-grained CSS control and overriding Verso's built-in CSS, I added syntax highlighting for both Lean and Prism.js clode blocks that fit the rest of the site.

I also improved the layout of the Post page (formally called Archive): I adjuested the heading hierarchy, placed categories horizontally,
and made the post list follow them directly, avoiding deep heading nesting.

# New Features

Beyond the styling, I also implemented several useful features. Any frontend code involved—CSS and JavaScript—was written by an LLM.

## Permalinks

Every heading below H2 now receives an automatically generated Verso `htmlId`.
Previously, anchors had to be added manually whenever I wanted to reference a partuclar heading.
Now every section gets its own permalink;
for example, [https://space.torus.icu/blog/2026-4-18-dependent-pattern-matching-and-convoy-patterns/#The-Problem](https://space.torus.icu/blog/2026-4-18-dependent-pattern-matching-and-convoy-patterns/#The-Problem)
takes you directly to the relevant section.

## Section Sidebar

I added a sidebar on the right that uses those automatically generated anchors to show the section currently in view, much like a table of contents.
Clicking a section title takes you there.

## Reading Progress

There's now a reading-progress indicator in the bottom right corner that displays a percentage.
Clicking it returns you to the top of the page.

## Estimated Reading Time

Each post now shows an estimated reading time in the form of `xx min read`, after its date and category.
Currently the estimate is very rough: it counts all words, including those inside code blosk, and divides the total by 225 words per minute.

## Footnotes

I found Verso's footnotes[^weird] unintuitive. They are now rendered as numbered references, and a foonote list is generated at the bottom of each post, similar to Markdown, with backlinks to the original references.

[^weird]: I didn't even finwd the documentation!

## Tables

While Verso's manual genere supports table rendering, blog genre doesn't.
The table syntax used by the manual genre is also based on nested lists, which I don't find ideal.
So I implemented Markdown-style pipe tables instead. For example, the following table definition (via code block)

 ```
 table +header (align := center)
 | Col1 | *Col 2* | Col 3 |
 | text | `code` | {typst}[$`f(x)`] |
 | {lean e}`Prop` | _apple_ | 2.2 |
 ```

is rendered as

 ```table +header (align := center)
 | Col1 | *Col 2* | Col 3 |
 | text | `code` | {typst}[$`f(x)`] |
 | {lean e}`Prop` | _apple_ | 2.2 |
 ```

The idea here is to use Lean parser combinators to parse a row block dedicated to tables, while recording the soure information for every cell.
Those positions need to be retained because a table may contain inline Lean code that still needs to be interpreted, and that code may depend on souce locations, e.g. diagnostics.

First, the pipe style table is parsed into an `Array (Array CellRange)`, where each `CellRange` contains only location information.
Then the text parser is run at those positions to obtain the cell contents together with their source information.
Those contens get passed to Verso's inline elaborator and then wrapped in block paragraphs. At that point, table parsing and elaboration are complete, producing `Array (Array Term)`, where `Term` is our familiar Lean syntax.

The table is then packaged into a custom Verso block extension called `blogTable`, which is ultimiately rendered as HTML in runtime.
Verso is genuinely powerful in terms of extensibility, though I find its API somewhat complicated and not very intuitive.

## Comments
I replaced utteranc.es with giscus and added styling consistent with the site's visual design.

# Feeds and SEO

1. An Atom feed. It traverses the rendered `Site`, extracts meaningful information such as post metadata and summaries, and uses it to generate `atom.xml`.

2. SEO improvements. The HTML templates in generator now includes Open Graph meta tags, including each post's summary and ~~my waifu's image~~ (TODO: implement the strikethrough...), and generates both `sitemap.xml` and `robots.txt`.

# Conclusion

Overall, the site now feels more like a properly built website rather than a collection of randomly generated HTML pages.

Think about it more carefully, Verso's whole model is pretty wild.
Evey post is a Lean source file. The post is parsed into Verso markup syntax, then elaborated into Lean expression defining a Verso document.
At runtime, that Lean expression is evaluated to produce the Verso document data, which is then rendered as HTML.
I think this is very much in the sprit of "Lean 4": excellent extensibility, extensive metaprogramming, everything implemented in Lean itself, and a fairly cursed pipeline that somehow produces quite good results.
I like the Gabriel's the MIT philosophy v.s. the New Jersey philosophy in his "The MIT Way: Worse Is Better" -- here I feel Lean 4 has MIT ambitions in theory and New Jersey engineering in practice :).
