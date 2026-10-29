import Blog.Site.Typst.Protocol
import VersoBlog

/-! Compiled SVG batches and the Verso components that display them. -/

open Lean Verso Genre Blog Output Html

namespace Blog.Typst

/-- A component's reference to one fragment in an installed SVG batch. -/
structure Payload where
  file : String
  /-- Zero-based index in the file's SVG batch. -/
  id : Nat
  display : Bool
  deriving ToJson, FromJson

private initialize outputsRef : IO.Ref (Std.HashMap String (Array String)) ←
  IO.mkRef {}

/-- Installs or replaces a file's indexed SVG outputs. -/
def install (file : String) (outputs : Array String) : IO Unit := do
  outputsRef.modify (·.insert file outputs)

/-- Renders an installed fragment with its inline or block wrapper. -/
def renderFragment (payload : Payload) : IO Html := do
  let some fragments := (← outputsRef.get)[payload.file]?
    | throw <| IO.userError s!"Typst batch is not installed for {payload.file}"
  let some svg := fragments[payload.id]?
    | throw <| IO.userError s!"Typst fragment index {payload.id} is missing from batch {payload.file} ({fragments.size} outputs)"
  if svg.isEmpty then return Html.empty
  return if payload.display then
    {{ <span class="typst-block">{{ Html.text false svg }}</span> }}
  else
    {{ <span class="typst-inline">{{ Html.text false svg }}</span> }}

/-- Component key shared by Typst role spans and fenced blocks. -/
def renderedTypst : Unit := ()

private def renderPayload (data : Json) : IO Html := do
  let payload ← match fromJson? (α := Payload) data with
    | .ok payload => pure payload
    | .error error => throw <| IO.userError s!"Invalid Typst fragment payload: {error}"
  renderFragment payload

/-- Resolves Typst component references in inline content. -/
@[inline_component renderedTypst]
def renderedTypstInline : InlineComponent where
  toHtml _ data _ _ := liftM (m := IO) (renderPayload data)

/-- Resolves Typst component references in block content. -/
@[block_component renderedTypst]
def renderedTypstBlock : BlockComponent where
  toHtml _ data _ _ _ := liftM (m := IO) (renderPayload data)

end Blog.Typst
