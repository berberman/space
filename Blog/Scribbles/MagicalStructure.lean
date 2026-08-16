import VersoBlog
import Blog.Site.Extensions
import Blog.Categories

open Verso Genre Blog

#doc (Post) "Magical Structure" =>

%%%
authors := ["berberman"]
date := {year := 2026, month := 8, day := 15}
categories := [Category.lean]
%%%

```leanInit empty
```

今天学习了一些 Lean 4 里 [structure elaborator](https://github.com/leanprover/lean4/blob/d97bd7be9a8c0cb33e21367c88db9cd9326011b1/src/Lean/Elab/Structure.lean) 的实现，
实际上 structure 比预想的要复杂得多！这里记录一些有趣的现象，没用的知识又增加了.jpg。

# 原始 Structure

来看一个最基本的 structure（没有继承、默认值、依赖类型）:

```lean empty
structure A where
  x : Nat
  y : Bool
```

Structure 只存在于 elaborator，而到 kernel 里它就是一个普通的 inductive type，类似：

```lean empty
inductive A' where
  | mk (x : Nat) (y : Bool)
```

所以：

```lean empty (name := A)
#check A.mk
#check A.rec
```

```leanOutput A
A.mk (x : Nat) (y : Bool) : A
```

```leanOutput A
A.rec.{u} {motive : A → Sort u} (mk : (x : Nat) → (y : Bool) → motive { x := x, y := y }) (t : A) : motive t
```

```lean empty (name := A')
#check A'.mk
#check A'.rec
```

```leanOutput A'
A'.mk (x : Nat) (y : Bool) : A'
```

```leanOutput A'
A'.rec.{u} {motive : A' → Sort u} (mk : (x : Nat) → (y : Bool) → motive (A'.mk x y)) (t : A') : motive t
```

基本上是能匹配上的。此外，structure 还会生成 projections：

```lean empty (name := A.a)
#check A.x

#check A.y
```

```leanOutput A.a
A.x (self : A) : Nat
```

```leanOutput A.a
A.y (self : A) : Bool
```

## Info Tree

Hover over `x`/`y` 时显示的信息是生成的 projection constans，而不是变量 `x`/`y`：

```leanInit empty
```

```lean empty
set_option trace.Elab.info true
```

```lean empty (name := A)
structure A where
  x : Nat
  y : Bool
```

可以点展开这个 trace 看到细节：

```leanOutput A
[Elab.info]
  • [Command] @ ⟨94, 0⟩-⟨96, 10⟩ @ Lean.Elab.Command.elabDeclaration
    • [Term] Type : Type 1 @ ⟨94, 0⟩†-⟨96, 10⟩† @ Lean.Elab.Term.elabSort
    • [Term] A (isBinder := true) : Type @ ⟨94, 10⟩-⟨94, 11⟩
    • [Term] Nat : Type @ ⟨95, 6⟩-⟨95, 9⟩ @ Lean.Elab.Term.elabIdent
      • [Completion-Id] Nat : some Sort.{?_uniq.4} @ ⟨95, 6⟩-⟨95, 9⟩
      • [Term] Nat : Type @ ⟨95, 6⟩-⟨95, 9⟩
    • [Term] Bool : Type @ ⟨96, 6⟩-⟨96, 10⟩ @ Lean.Elab.Term.elabIdent
      • [Completion-Id] Bool : some Sort.{?_uniq.6} @ ⟨96, 6⟩-⟨96, 10⟩
      • [Term] Bool : Type @ ⟨96, 6⟩-⟨96, 10⟩
    • [Term] A.x (isBinder := true) : A → Nat @ ⟨95, 2⟩-⟨95, 3⟩
    • [Term] A.y (isBinder := true) : A → Bool @ ⟨96, 2⟩-⟨96, 3⟩
    • [Term] A (isBinder := true) : Type @ ⟨94, 10⟩-⟨94, 11⟩
```

# 继承

Structure 支持继承：

```leanInit empty
```

```lean empty
structure A where
  x : Nat
  y : Bool

structure B extends A where
  z : String
```

这时 `B` 的构造器会包括一个 `toA : A` 包装了所有来自 `A` 的字段，
而不是把 `A` 里的所有字段都拷过来：

```lean empty (name := B.mk)
#check B.mk
```

```leanOutput B.mk
B.mk (toA : A) (z : String) : B
```

这个字段会生成出一个向上转换的 projection（在 type class 里向父类转换很有用）：

```lean empty (name := B.toA)
#check B.toA
```

```leanOutput B.toA
B.toA (self : B) : A
```

然后还会生成一个秘密的定义（内部实现细节，在 structure instance elaborator 里用到）：

```lean empty (name := B.mk._flat_ctor)
#check B.mk._flat_ctor
```

```leanOutput B.mk._flat_ctor
B.mk._flat_ctor (x : Nat) (y : Bool) (z : String) : B
```

可以看到它把 `A` 里的所有字段都添加进来了，而不是像 `B.mk` 那样添加一个 `toA : A`。

## 字段 Projection

在有继承的情况下并不会为来自 parent structure 的 field 生成 projections：

```lean empty +error (name := B.xyz)
#check B.x
#check B.y
#check B.z
```

```leanOutput B.xyz
Unknown constant `B.x`
```

```leanOutput B.xyz
Unknown constant `B.y`
```

```leanOutput B.xyz
B.z (self : B) : String
```

这时 field notation `b.x` 会通过 `toA.x` 实现：


```lean empty (name := b.x)
variable (b : B) in
set_option pp.fieldNotation false in

#check b.x
```

```leanOutput b.x
A.x (B.toA b) : Nat
```

## 多继承

在多继承时不同的继承顺序可能会导致生成出不同的构造器和 projections。

```leanInit empty
```

```lean empty
structure A where
  x : Nat
  y : Bool

structure B where
  y : Bool
  z : String
```

先继承 `A` 再继承 `B`：

```lean empty -keep (name := c1)
structure C extends A, B where
  p : Nat

#check C.mk

variable (c : C) in
set_option pp.fieldNotation false in

#check c.y
```

```leanOutput c1
C.mk (toA : A) (z : String) (p : Nat) : C
```

```leanOutput c1
A.y (C.toA c) : Bool
```

如果先继承 `B` 再继承 `A`，结果就不同了：


```lean empty -keep (name := c2)
structure C extends B, A where
  p : Nat

#check C.mk

variable (c : C) in
set_option pp.fieldNotation false in

#check c.y
```

```leanOutput c2
B.y (C.toB c) : Bool
```

```leanOutput c2
C.mk (toB : B) (x p : Nat) : C
```

可以看到这时 `c.y` 由 `toB.y` 提供而不是 `toA.y`。

# Structure Instance

Structure elaborator 实现的是如何 elaborate {leanKw}`structure` command；
而 structure instance elaborator 实现的是用于创建 structure 的 `{ ... }`。

> 我比较喜欢叫它 structure notation，不过源码里叫 structure instance。

回到刚才的这个多继承定义：

```leanInit empty
```

```lean empty
structure A where
  x : Nat
  y : Bool

structure B where
  y : Bool
  z : String

structure C extends A, B where
  p : Nat
```

如果要创建一个 `C`：

```lean empty -keep
def c : C := { x := 1, y := false, z := "qwq", p := 233 }
```

要记住 `{ ... } ` 的 elaboration 总是取决于 expected type。
上面我们有 `c : C` 所以成功了。

或者也可以用这个神秘的语法在结尾指定 expected type：

```lean empty (name := c2)
#check { x := 1, y := false, z := "qwq", p := 233 : C}
```

```leanOutput c2
{ x := 1, y := false, z := "qwq", p := 233 } : C
```

也可以像这样以 `a` 为基础添加 `z` 和 `p` 两个字段来创建一个新的 `c`：

```lean empty -keep
def a : A := { x := 1, y := false }
def c : C := { a with z := "qwq", p := 233 }
```

当然这样是不行的，因为 `b` 里没有 `x`，`c` 从 `b` 创建需要手动指定 `x`：

```lean empty -keep (name := c4) +error
def a : A := { x := 1, y := false }
def b : B := { a with z := "qwq" }
def c : C := { b with p := 233 }
```

```leanOutput c4
Fields missing: `x`

Hint: Add missing fields:

  ̲ ̲ ̲ ̲ ̲ ̲ ̲ ̲ ̲ ̲ ̲ ̲ ̲ ̲ ̲ ̲ ̲ ̲ ̲ ̲ ̲ ̲ ̲x̲ ̲:̲=̲ ̲_̲
```

同时从 `a` 和 `b` 创建就好了：

```lean empty -keep
def a : A := { x := 1, y := false }
def b : B := { a with z := "qwq" }
def c : C := { a, b with p := 233 }
```

也就是说 {leanKw}`{ ... with ... }` 语法中 {leanKw}`with` 前面的 structures 可以包含
在 expected type 中 structure 里不存在的多余的字段，而它们会被丢弃：

```lean empty -keep
def a : A := { x := 1, y := false }
def b : B := { a with z := "qwq" }

def a' : A := { a, b with }
```
不过 {leanKw}`with` 后面的字段必须在 expected type 中 structure 里存在才行：
```lean empty -keep +error (name := a')
def a : A := { x := 1, y := false }
def b : B := { a with z := "qwq" }

def a' : A := { a, b with z := "pwp"}
```

```leanOutput a'
`z` is not a field of structure `A`
```
## Hoist

如果我们好奇，打印出这种 {leanKw}`{ ... with ... }` 创建的 structure：

```lean empty -keep (name := c6)
def a : A := { x := 1, y := false }
def b : B := { a with z := "qwq" }
def c : C := { a, b with p := 233 }

#print c
```

```leanOutput c6
def c : C :=
have __src := a;
have __src_1 := b;
{ toA := __src, z := __src_1.z, p := 233 }
```

整个定义变成了一个嵌套的 {leanKw}`have` 表达式，把 `a` 和 `b` 绑定到了 `__src` 和 `__src_1` 上。
把 `a` 和 `b` hoist 到 `{...}` 外可以避免 elaborate 它们多次以及减少不必要的多次计算（Lean 并不是 call-by-need 的）。

事实上在 structure instance elaborator 生成的 syntax 中它们都是 {leanKw}`let` 表达式，
这个例子因为 term elaborator 发现这些 {leanKw}`let` 都是 nondependent 的，即 body 类型不依赖 {leanKw}`let` 表达式，
把它们优化成了 {leanKw}`have`.

值得一提的是有个非常坏的一点：下划线前缀 `__` 是有特殊语义的（{lean empty}`Lean.Name.isImplementationDetail`）——
在 elaboration 过程中 {lean empty}`Lean.Meta.whnf` 总是会 ζδ reduce {leanKw}`let`，无视其他条件。

# 默认值

## 函数

一般函数的默认值是通过 {lean empty}`optParam` 记录在参数类型中的：


```lean empty (name := foo)
def foo (x : Nat := 233) := x

#print foo
```

```leanOutput foo
def foo : optParam Nat 233 → Nat :=
fun x => x
```

当参数没有提供时 elaborator 会尝试填入默认值。


## Structure

类似，structure 的字段可以有默认值。
默认值是可以依赖已定义的字段的——已定义字段会在 local context 中变成 fvar，在定义后续字段时可以直接被引用。
Structure elaborator 会在默认值 projection 中处理他们。

```leanInit empty
```

```lean empty
structure A where
  x : Nat
  y : Nat := x

structure B extends A where
  z : Nat → Nat := fun _ => x + y
```

但是，这个默认值并不通过 {lean empty}`optParam` 记录在构造器中：

```lean empty (name := AB.mk)
#check A.mk
#check B.mk
```

```leanOutput AB.mk
A.mk (x y : Nat) : A
```

```leanOutput AB.mk
B.mk (toA : A) (z : Nat → Nat) : B
```

而在遇到默认值时 structure elabrator 会生成特殊的默认值 projection：

```lean empty (name := defa)
#print A.y._default
```

```leanOutput defa
def A.y._default : Nat → Nat :=
fun x => id x
```

注意到这里有一个看起来毫不相干的 {lean empty}`id` 在函数 body 的开始。

```lean empty (name := defb)
#print B.z._default
```
```leanOutput defb
def B.z._default : Nat → Nat → Nat → Nat :=
fun x y => id fun x_1 => x + y
```

事实上它不一定在开始，而是在传入字段和返回的默认值中间作为隔断。
这纯粹是个编码技巧：首先默认值可能依赖任意多个前序字段，所以这个 projection 得接受它们作为参数；
其次一个字段可能是一个函数，本身就需要传入参数。
这就导致在生成定义时需要在依赖字段和返回值之前隔断一下，不然这个 projection 就会融合成一个大函数，
让 elaborator 不知道该填入前多少个依赖的字段了。

类似之前说过的 `_flat_ctor`，这些 projections 是给 structure instance elabrator 使用的，永远不应该被手动调用。

## 菱形继承

来看一个更复杂的例子！

```leanInit empty
```

```lean empty
structure A where
  x : Nat
  y : Nat := x

structure B extends A where
  z : Nat := x + 1

structure C extends A where
  z : Nat := y + 2

structure D extends B, C
```

回顾一下之前说过的，我们观察一下 `D` 的构造器：

```lean empty (name := D.mk)
#check D.mk
```

```leanOutput D.mk
D.mk (toB : B) : D
```

它只包含一个 `toB` 字段，因为 `B` 和 `C` 的字段完全相同，而 `B` 先被继承。

```lean empty (name := d)
def d : D := { x := 1 }

#reduce d.x
#reduce d.y
#reduce d.z
```

```leanOutput d
1
```

```leanOutput d
1
```

```leanOutput d
2
```

类似，因为 `B` 比 `C` 先被继承，所以 `d.z` 得到了 `B` 中定义的 `x + 1`（`2`），而不是 `C` 中定义的 `x + 2`（`3`）。
