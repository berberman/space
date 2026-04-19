import VersoBlog
import Blog.Categories
import Blog.Meta

import Blog.Posts.HEqAndAxiomK

open Verso Genre Blog

#doc (Post) "Dependent Pattern Matching and Convoy Patterns" =>

%%%
authors := ["berberman"]
date := {year := 2026, month := 4, day := 18}
categories := [Category.lean]
%%%

```leanInit empty
```

The other day, I saw my friend [CircuitCoder](https://c-3.moe/) ask a [Rocq](https://rocq-prover.org/) question in a group chat:

"How do you prove the following theorem without using the `dependent destruction` tactic? It seems to require the Convoy Pattern, which I tried learning but still don't quite grasp."

```coq
Inductive vector (A : Type) : nat -> Type :=
  | vnil : vector A 0
  | vcons {n} (v : vector A n) (a : A) : vector A (S n).

Lemma test {A} {n} {v : vector A (S n)} :
  exists v' : vector A n, exists a : A, v = vcons A v' a.
```

Let's explore dependent pattern matching and the convoy pattern in Lean!

# HList

Let's consider this heterogeneous list from Chapter 9.2 of _Certified Programming with Dependent Types_ (CPDT):

```lean empty
inductive HList {α : Type u} {β : α → Type u} : List α → Type u where
  | nil : HList []
  | cons {x xs} : β x → HList xs → HList (x :: xs)

inductive Member {α : Type u} (x : α) : List α → Type u where
  | head {xs} : Member x (x :: xs)
  | tail {x' xs} : Member x xs → Member x (x' :: xs)
```

Unlike {leanInline empty}`List.Mem`, here the membership type is inhabited in the universe {leanInline (universes := "u") empty}`Type u` instead of {leanInline empty}`Prop`.

## The Problem

Now let's define a get function for `HList`:

```lean empty +error (name := HList.get.nil)
def HList.get {α β x} {xs : List α} (mls : @HList α β xs) (m : @Member α x xs) : β x :=
  match mls with
  | .nil => _
  | .cons y ys => match m with
    | .head => y
    | .tail m' => get ys m'
```

For the `.cons case`, we can pattern match on membership and recursively look for the element we want because membership encodes the position of that element.
But what should we do for the `.nil` case? Here is the goal state:

```leanOutput HList.get.nil
don't know how to synthesize placeholder
context:
α : Type u_1
β : α → Type u_1
x : α
xs : List α
mls : HList xs
m : Member x []
⊢ β x
```

We have a false assumption `m : Member x []`, which states that x is in the empty list.
`by cases m` perfectly closes the goal because the two constructors of `Member` both return non-empty lists in their indices.
There is simply no way to construct a `Member ... []`.


However, let's say we don't want to use tactics to "cheat." Can we still pattern match on `m` just like we did in the `.cons` case?

```lean empty +error (name := HList.get.nil.ht)
def HList.get {α β x} {xs : List α} (mls : @HList α β xs) (m : @Member α x xs) : β x :=
  match mls with
  | .nil => match m with
    | .head => sorry
    | .tail _ => sorry
  | .cons y ys => match m with
    | .head => y
    | .tail m' => get ys m'
```

Unfortunately, even with `sorry`, `| .head => sorry` raises an error:

```leanOutput HList.get.nil.ht
Type mismatch
  Member.head
has type
  Member ?m.9 (?m.9 :: ?m.10)
but is expected to have type
  Member x []
```

It seems Lean fails to recognize this as an impossible case and complains about a unification failure.
But in fact, we can just omit the `.nil` case entirely, and Lean will figure out it's impossible!

```lean -keep empty
def HList.get {α β x} {xs : List α} (mls : @HList α β xs) (m : @Member α x xs) : β x :=
  match mls with
  | .cons y ys => match m with
    | .head => y
    | .tail m' => get ys m'
```

Alternatively, we can explicitly merge `m` as a second discriminant (which `match mls with` already does under the hood).

```lean -keep empty
def HList.get {α β x} {xs : List α} (mls : @HList α β xs) (m : @Member α x xs) : β x :=
  match mls, m with
  | .cons y ys, .head => y
  | .cons _ ys, .tail m' => get ys m'
```

## Convoy Pattern, with K

But what if we want to write the terms manually to declare this function?

One possible fix is to insert an equality proof during pattern matching, asserting that the list in `Member`'s index is equal to `[]`.
This creates an absurd proof `x :: xs = []` that we can then eliminate. The Rocq community calls this trick the "convoy pattern,"
and it seems to originate from CPDT, though I've never found a precise definition of it...

Actually, this is similar to what the cases tactic does. Since `match (motive := ...)` doesn't seem to support a function as a motive as of today,
let's directly use the pattern match function generated for this inductive type:

```lean empty (name := Member.casesOn)
#check Member.casesOn
```

```leanOutput Member.casesOn
Member.casesOn.{u_1, u} {α : Type u} {x : α} {motive : (a : List α) → Member x a → Sort u_1} {a✝ : List α}
  (t : Member x a✝) (head : {xs : List α} → motive (x :: xs) Member.head)
  (tail : {x' : α} → {xs : List α} → (a : Member x xs) → motive (x' :: xs) a.tail) : motive a✝ t
```

We could do:

```lean empty +error (name := HList.get.nil.convoy)
def HList.get {α β x} {xs : List α} (mls : @HList α β xs) (m : @Member α x xs) : β x :=
  match mls with
  | .nil =>
    m.casesOn
      (motive := fun lst _ => lst = [] → β x)
      (fun h => _)     -- h : x :: xs✝ = []
      (fun _ h => _)   -- h : x'✝ :: xs✝ = []
      rfl
  | .cons y ys => match m with
    | .head => y
    | .tail m' => get ys m'
```

The goal state for `.head` is:

```leanOutput HList.get.nil.convoy
don't know how to synthesize placeholder
context:
α : Type u_1
β : α → Type u_1
x : α
xs : List α
mls : HList xs
m : Member x []
xs✝ : List α
h : x :: xs✝ = []
⊢ β x
```

and the goal state for `.tail` is:

```leanOutput HList.get.nil.convoy
don't know how to synthesize placeholder
context:
α : Type u_1
β : α → Type u_1
x : α
xs : List α
mls : HList xs
m : Member x []
x'✝ : α
xs✝ : List α
x✝ : Member x xs✝
h : x'✝ :: xs✝ = []
⊢ β x
```

The `lst` in the motive are substituted with the indices of those two constructors for `Member`: `x :: xs✝` and `x'✝ :: xs✝` respectively,
corresponding to the two match cases.

Now we can feed those false equalities to {leanInline empty}`List.noConfusion`.
Lean will figure out it's impossible for `.cons` to equal `.nil`, allowing us to prove anything. `<|` is Lean version of `$` to avoid parentheses.

```lean -keep empty
def HList.get {α β x} {xs : List α} (mls : @HList α β xs) (m : @Member α x xs) : β x :=
  match mls with
  | .nil =>
    m.casesOn
      (motive := fun lst _ => lst = [] → β x)
      (fun h => List.noConfusion rfl <| heq_of_eq h)
      (fun _ h => List.noConfusion rfl <| heq_of_eq h)
      rfl
  | .cons y ys => match m with
    | .head => y
    | .tail m' => get ys m'
```

*Bonus*: The `match` in the `.cons` case can be rewritten similarly:

```lean -keep empty
def HList.get {α β x} {xs : List α} (mls : @HList α β xs) (m : @Member α x xs) : β x :=
  match mls with
  | .nil =>
    m.casesOn
      (motive := fun lst _ => lst = [] → β x)
      (fun h => List.noConfusion rfl <| heq_of_eq h)
      (fun _ h => List.noConfusion rfl <| heq_of_eq h)
      rfl
  | @cons _ _ x' xs' y ys =>
    m.casesOn
      (motive := fun lst _ => lst = x' :: xs' → β x)
      (fun h => List.noConfusion rfl (heq_of_eq h) (fun hx _=> eq_of_heq hx ▸ y))
      (fun m' h => List.noConfusion rfl (heq_of_eq h) (fun _ hxs => get (eq_of_heq hxs ▸ ys) m'))
      rfl
```

Here, we insert `lst = x' :: xs'` into the motive to obtain the equalities `x = x'` in the `head` case and `xs✝ = xs'` in the `tail` case.
`▸` is the "cast" notation, similar to {leanInline empty}`Eq.subst`.
Without these equalities, we'd run into the issue that the element we extracted from the correct position cannot be proven to be our desired element.

### Convoy Pattern, without K

It's worth mentioning that this trick leverages Lean's proof irrelevance (converting heterogeneous equality to homogeneous equality),
while CPDT uses a completely different approach. Translated to Lean, it would look something like this:

```lean -keep empty
def HList.get {α β x} {xs : List α} (mls : @HList α β xs) (m : @Member α x xs) : β x :=
  match mls with
  | .nil =>
    m.casesOn
      (motive := fun lst _ =>
        match lst with
        | [] => β x
        | _ :: _ => Unit
      )
      ()
      (fun _ => ())
  | .cons y ys =>
    m.casesOn
      (motive := fun lst _ =>
        match lst with
          | [] => Empty
          | x' :: xs' => β x' → (Member x xs' → β x) → β x
      )
      (fun y _ => y)
      (fun m' _ f => f m')
      y
      (get ys)
```

We no longer insert equality proofs into the motive; instead, we compute the result type dynamically based on the input in the motive.
This is called _large elimination_ in dependent type theory, as we are eliminating `Member` of {leanInline (universes := "u") empty}`Type u` into another type which resides in {leanInline (universes := "u") empty}`Type (u + 1)`.

* In the `.nil` case, lst is always `.cons` in the two matching cases, so we simply return Unit.
  Meanwhile, since `xs = []` here, the type of this entire casesOn expression evaluates to `β x`!
* The `.cons` case is a bit more interesting—we pass the function itself into the motive to make the types match in the recursive case.
  (In fact, the {leanInline empty}`Empty` is not important, and we could use any other type, as `lst` in this case is always `.cons`.)

# Vec

Now that we have a better understanding of the original problem, let's try to prove it in Lean. First, let's translate `Vec` to Lean:

```lean empty
inductive Vec (α : Type) : Nat → Type where
  | nil : Vec α 0
  | cons {n} (x : α) (xs : Vec α n) : Vec α (n + 1)
```

Let's try to pattern match on `v` just like we would in Rocq:

```lean empty +error (name := Vec.match.nil)
example {α : Type} {n : Nat} (v : Vec α (n + 1)) : ∃ (v' : Vec α n) (x : α), v = .cons x v' :=
  match v with
  | .nil => _
  | .cons x_ v_ => sorry
```

However, unlike in Rocq, the `.nil` case immediately throws an error (just as we saw earlier with `HList`):

```leanOutput Vec.match.nil
Type mismatch
  Vec.nil
has type
  Vec ?m.14 0
but is expected to have type
  Vec α (n + 1)
```

This means Lean knows this case is impossible, so we only need to complete the proof for `.cons`:

```lean empty
example {α : Type} {n : Nat} (v : Vec α (n + 1)) : ∃ (v' : Vec α n) (x : α), v = .cons x v' :=
  match v with
  | .cons x_ v_ => ⟨v_, ⟨x_, rfl⟩⟩
```

The frustrating issue in Rocq simply doesn't exist in Lean.
Since we know that pattern matching eventually elaborates into a recursor call, here we have the recursor for `Vec`:

```lean empty (name := Vec.rec)
#check Vec.rec
```

```leanOutput Vec.rec
Vec.rec.{u} {α : Type} {motive : (a : Nat) → Vec α a → Sort u} (nil : motive 0 Vec.nil)
  (cons : {n : Nat} → (x : α) → (xs : Vec α n) → motive n xs → motive (n + 1) (Vec.cons x xs)) {a✝ : Nat}
  (t : Vec α a✝) : motive a✝ t
```

We should be able to manually use this recursor to complete the proof, just like we did with the HList exercise earlier.
How do we choose the motive?

The first thought is to directly eliminate `v` into our target type (`∃ v' x, v = Vec.cons x v'`),
meaning `motive := fun _n _v => ∃ v' x, v = Vec.cons x v'`.
However, if we actually do this, we'll get stuck on the `.nil` case even though `.cons` works directly:

```lean empty +error (name := Vec.rec.nil)
example {α : Type} {n : Nat} (v : Vec α (n + 1)) : ∃ (v' : Vec α n) (x : α), v = .cons x v' :=
  Vec.rec
    (motive := fun n_ v_ => ∃ v' x, v = Vec.cons x v')
    _
    (fun _ _ h => h)
    v
```

```leanOutput Vec.rec.nil
don't know how to synthesize placeholder for argument `nil`
context:
α : Type
n : Nat
v : Vec α (n + 1)
⊢ (fun n_ v_ => ∃ v' x, v = Vec.cons x v') 0 Vec.nil
```

So let's try using `motive := fun n_ v_ => n + 1 = n_ → ∃ v' x, v = Vec.cons x v'` as we did before.
This way, in the `.nil` case, we get a contradiction: `n + 1 = 0`:

```lean empty +error (name := Vec.rec.nil.convoy)
example {α : Type} {n : Nat} (v : Vec α (n + 1)) : ∃ (v' : Vec α n) (x : α), v = .cons x v' :=
  Vec.rec
    (motive := fun n_ v_ => n + 1 = n_ → ∃ v' x, v = Vec.cons x v')
    (fun h => Nat.noConfusion h)
    (fun _ _ h₁ h₂ => h₁ h₂)
    v
    rfl
```

Indeed, .nil is solved, but now `.cons` is broken: after fixing the index `n_` to `n + 1`, `h₁ : n + 1 = n✝ → ∃ v' x, v = Vec.cons x v'`
becomes a useless assumption because here `n + 1 = n✝ + 1`.

We need a different approach.
Notice that the index `v_` will eventually be replaced by `Vec.cons x_ xs_`.
If we insert another equality `v ≍ v_`, we can get `v ≍ Vec.cons x_ xs_`.
We must use {leanInline empty}`HEq` here because `v` has type `Vec α (n + 1)` while `v_` has type `Vec α n_`;
until we utilize `n + 1 = n_`, their types are different, making standard {leanInline empty}`Eq` invalid.

```lean empty
example {α : Type} {n : Nat} (v : Vec α (n + 1)) : ∃ (v' : Vec α n) (x : α), v = .cons x v' :=
  Vec.rec
    (motive := fun n_ v_ => n + 1 = n_ → v ≍ v_ → ∃ v' x, v = Vec.cons x v')
    (fun h _ => Nat.noConfusion h)
    (fun x_ xs_ h₁ h₂ h₃ => by
      have := Nat.add_right_cancel h₂
      subst this
      have := eq_of_heq h₃
      exists xs_, x_
    )
    v
    rfl
    HEq.rfl
```

Here, tactics help us handle some of the tedious steps. If you prefer, you can also manually eliminate this heterogeneous equality:

```lean empty
example {α : Type} {n : Nat} (v : Vec α (n + 1)) : ∃ (v' : Vec α n) (x : α), v = .cons x v' :=
  Vec.rec
    (motive := fun n_ v_ => n + 1 = n_ → v ≍ v_ → ∃ v' x, v = Vec.cons x v')
    (fun h _ => Nat.noConfusion h)
    (fun x_ xs_ _ h h' =>
      Eq.ndrec
        (motive := fun n'' => (t : Vec α n'') → v ≍ Vec.cons x_ t → ∃ v' x, v = Vec.cons x v')
        (fun t g => ⟨t, ⟨x_, eq_of_heq g⟩⟩)
        (Nat.add_right_cancel h)
        xs_
        h'
    )
    v
    rfl
    HEq.rfl
```

Similar to before, we relied on {leanInline empty}`eq_of_heq`—or effectively Axiom K (as explained in my {page_link Blog.Posts.HEqAndAxiomK}[previous post])—to complete the proof using the convoy pattern.

The version using large elimination looks like this:

```lean empty
example (α : Type) (n : Nat) (v : Vec α (n + 1)) : ∃ v' x, v = Vec.cons x v' :=
  Vec.rec
    (motive := fun m =>
      Nat.rec
        (motive := fun m' => Vec α m' → Prop)
        (fun _ => True)
        (fun _ _ v_ => ∃ v' x, v_ = Vec.cons x v') m
    )
    trivial
    (fun x_ xs_ _ => ⟨xs_, x_, rfl⟩)
    v
```

Here, in the motive of `Vec.rec`, we dynamically compute the {leanInline empty}`Prop` to eliminate into based on the index `m`.

* If `m = 0`, we need to prove {leanInline empty}`True`, which is proven by {leanInline empty}`True.intro` or simply {leanInline empty}`trivial`.
* Otherwise, we need to prove `∃ v' x, v_ = Vec.cons x v'`, where `v_` will be replaced by `Vec.cons x_ xs_`. Setting `v' = xs_` and `x = x_` completes the existential proof.

I think this approach is much cleaner.
It doesn't require introducing extra equality proofs just to eliminate them later, and it seems to avoid using Axiom K altogether, though I am not entirely certain.
