import VersoBlog

open Verso Genre Blog
open Verso Output Html

block_component +directive academicHero where
  toHtml id _data _goI goB contents := do
    pure {{
      <div class="academic-hero" id={{id}}>
        {{← contents.mapM goB}}
      </div>
    }}

block_component +directive academicIntro where
  toHtml id _data _goI goB contents := do
    pure {{
      <div class="academic-intro" id={{id}}>
        {{← contents.mapM goB}}
      </div>
    }}

block_component +directive academicPhoto (src : String) (alt : String) where
  toHtml id _data _goI _goB _contents := do
    pure {{
      <figure class="academic-photo" id={{id}}>
        <img src={{src}} alt={{alt}}/>
      </figure>
    }}

block_component +directive academicSection (title : String) where
  toHtml id _data _goI goB contents := do
    pure {{
      <section class="academic-section" id={{id}}>
        <h2 class="academic-section__title">{{title}}</h2>
        <div class="academic-section__body">
          {{← contents.mapM goB}}
        </div>
      </section>
    }}

#doc (Page) "Academic Profile" =>
%%%
%%%

::::academicHero

:::academicIntro

TODO1
TODO1

TODO1
TODO1


TODO1
TODO1

:::

:::academicPhoto "/static/profile-placeholder.svg" "Profile photo placeholder"

Photo placeholder.

:::

::::

:::academicSection "Research Interests"

- TODO 1
- TODO 2
- TODO 3

:::

:::academicSection "Publications"


- TODO 1
- TODO 2
- TODO 3

:::

:::academicSection "Education"


- TODO 1
- TODO 2
- TODO 3

:::

:::academicSection "Work Experience"


- TODO 1
- TODO 2
- TODO 3

:::

:::academicSection "Selected Projects"


- TODO 1
  - 1.1
  - 1.2
  - 1.3
- TODO 2
  - 2.1
  - 2.2
  - 2.3

:::

:::academicSection "Contact"


- TODO 1
- TODO 2
- TODO 3

:::
