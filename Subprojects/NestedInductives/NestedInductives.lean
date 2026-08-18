import SubVerso.Examples
import Lean

open SubVerso.Examples
open Lean Meta Elab

%example Foo
inductive Foo (b : Bool) (α : Type) where
  | mk (x : α) : Foo b α
%end

%signature FooType
  Foo (b : Bool) (α : Type) : Type

%example bug
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
%end

%example Bar
#bug
%end

%example Bar.rec
#print Bar.rec
%end

%example version
#eval Lean.versionString
%end
