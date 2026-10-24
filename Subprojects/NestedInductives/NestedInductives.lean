import Lean

open Lean Meta Elab

-- ANCHOR: Foo
inductive Foo (b : Bool) (α : Type) where
  | mk (x : α) : Foo b α
-- ANCHOR_END: Foo

-- ANCHOR: bug
elab "#bug" : command => Command.liftTermElabM do
  let indType : InductiveType := {
      name := `Bar
      type := .sort 1
      ctors := [
        { name := `Bar.mk
          type := ← mkArrow (mkApp2 (.const `Foo [])
                    (mkNatLit 1) (.const `Bar [])) (.const `Bar []) }
      ]
    }
  let decl := Declaration.inductDecl [] 0 [indType] false
  addDecl decl
-- ANCHOR_END: bug

-- ANCHOR: Bar
#bug
-- ANCHOR_END: Bar

-- ANCHOR: Bar.rec
#print Bar.rec
-- ANCHOR_END: Bar.rec

-- ANCHOR: version
#eval Lean.versionString
-- ANCHOR_END: version
