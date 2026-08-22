import VersoBlog
import Blog.Site.Extensions
import Blog.Categories

open Verso Genre Blog

#doc (Post) "The Recent Kernel Soundness Bug and Nested Inductive Types" =>

%%%
authors := ["berberman"]
date := {year := 2026, month := 8, day := 17}
categories := [Category.lean]
%%%

```leanInit empty
```

> Disclaimer：本文作者还 too young, too simple, sometimes naive，请读者 feel free 跳过第一部分的暴论。

# 社区反应

最近 kernel 爆出了个 [nested inductive type soundness 漏洞](https://github.com/leanprover/lean4/issues/14576)，在整个社区引发了轩然大波。
我主要是从 [Lean Zulip](https://leanprover.zulipchat.com/#narrow/channel/270676-lean4/topic/Counterexample.20to.20the.20Lean.20Conjecture.20.28Soundness.20Bug.29/near/613135216)
这里大概看到了事件的全貌，估计其他社交媒体上也有很多类似的新闻和讨论；
Leo 在[修复](https://github.com/leanprover/lean4/pull/14577)之后也发了一篇[事后分析](https://leodemoura.github.io/blog/2026-8-1-postmortem-for-kernel-soundness-bug-14576/)。

其实时间线不是很复杂，（按太平洋时区）大概是：
- 7 月 25 日 [Ramana](https://github.com/xrchz) 说自己在 Lean 里[否证了 Collatz conjecture](https://github.com/xrchz/collatzlean)。
  大伙觉得不对劲，这怎么能证否呢，估计是 kernel 出错了。
- 7 月 25 日 [Kiran](https://github.com/kiranandcode) 用一些奇妙构造整出了 [MWE]((https://github.com/leanprover/lean4/issues/14576)，能直接证出 {lean empty}`False`。
- 7 月 28 日 [Leo](https://github.com/leodemoura) [修复](https://github.com/leanprover/lean4/pull/14577)。

我认为技术上来说这是个非常小的 kernel 实现细节被遗漏了，基本上无伤大雅（后面我们会复现一个小的演示），也和 Lean 元理论的正确性无关。
有意思的是碰巧 [nanoda](https://github.com/ammkrn/nanoda_lib) 也因为类似的 bug 可以接受这个证明，虽然它们原理不完全一样。

不过大家都反应可是炸开了花，甚至有种“Lean 多年来的理论是否是错误的”，“Lean 的数学大厦轰然倒塌”，“我们还能信任 Lean 吗” 之类的疑问。
Zulip 里那个讨论也混入了很多相当多的离题非技术争论。按时间顺序讨论大概有这些：

1. 为什么这种严重的 kernel bug 为什么存在这么久，是否应该用 AI 主动 audit kernel？
2. 既然 nanoda 也可以接受这个证明， external kernel checker 的意义是什么？
3. 这种神秘的证明是怎么来的，是人类的创作吗？
4. 这种 soundness bug 应该以最小可复现的形式汇报，而不是扔一个带有错误证明的数学定理上来。
5. 既然存在 [Lean4Lean](https://github.com/digama0/lean4lean) 这种对 Lean 自己的形式化验证，为什么没能找出 bug？
6. Kernel 有没有正确实现类型系统，还是说类型系统本身的元理论就是烂的？
7. Soundness bug 是 security bug 吗，是 0-day 吗？
8. 应该继续发展 Lean4Lean 的形式化，还是大力出奇迹直接在 kernel 里硬找 bug？
9. [Lean FRO](https://lean-lang.org/fro/) 组织架构适合和学术界合作研究吗，比如搞 AI 证明的能人拿到好多钱，但是做类型论的还是穷逼（悲）。
10. AI 时代信任模型应该是什么，即使 state 出来证出来就一定对吗？

看完了我整个人都麻了.jpg
先不说这件事和 AI 关系有多大（虽然确实挺大的，毕竟这个 Collatz conjecture 证明看起来就是 AI 在做），
大伙疑似有点太 hype 了，包括 AI 使用以及 Lean 本身的 soundness 问题上。

我个人完全不反对用 AI，很多时候它们真的能帮助我们思考并解决问题；
但是我现在有种感觉（或许是错觉），就是有些人都在用 AI 使劲地尝试证出东西，成功之后宣称他们获得了巨大形式化成就，
尽管他们可能对要证的东西本身理解都不到位。

这些工作当然是很有价值的，但是每次看到这种我就感觉，有些失落……
或许和传统软件开发被 AI 取代这种担心类似，AI 真的在很多地方拥有更好的直觉，尤其是代数和逻辑方向。
我害怕在这种流行下精心的理论发展（包括定理证明系统的理论研究）变成了不必要的过程——有什么问题全扔给 AI 大力出奇迹就行了，
直不直觉的无所谓。这样以来学习探索并掌握一些想法的精妙之处也变得作用甚微。

据我所知数学社区的意见是 AI 证明只要是正确 state 并 prove 出来就是可接受的——
很有道理，我们的确完成了形式化，证明我们目标理论是正确的；
但是从另一个角度看，AI 产出的证明过程很多时候都不是人类可读的，
就像软件工程的火葬场那样，one-shot 且不可维护。
不禁让我扪心自问：我们真的能从这样的形式化的过程中学到更多，还是说形式化本身变成了一个纯粹的目的。

话说回来这个 MWE 的确很有意思，使用了很精妙的做法证出了 {lean empty}`False`：
让 {lean empty}`true` 和 {lean empty}`false` 发生哈希碰撞、绕过一些 fast path、
然后再利用缓存使错误的结果被复用，最终才能和真正 nested inductives 的 bug 联系起来。
说实话我觉得这并非人类能轻松想出的办法。

Lean 创造出来是为了帮助大家做形式化验证的，
当然 kernel 最好是既 sound 又 complete，
我觉得有些人把努力放在了找出可能存在的一些非常微小的、几乎没影响有限漏洞上尝试攻破 Lean 的理论，我不好说。

# Mutual Inductive Types

暴论结束。在深入 _nested_ inductive types 之前，
我们先看看  _mutual_ inductive types 的处理。

和先前版本不同，Lean 4 的 kernel 是原生支持 _mutal_ inductive 的：

```leanInit e
```

```lean e -keep
mutual

inductive A where
  | mk (b : B)

inductive B where
  | mk (a : A)

end
```

这个例子不是很有趣，因为 `A` 和 `B` 都不 inhabited。

然后有一些限制，比如 {leanKw}`mutual` block 内的 inductive parameters 必须一致：

```lean e -keep +error (name := err1)
mutual

inductive A (α : Type) where
  | mk (b : B)

inductive B (α : Type) (n : Nat) where
  | mk (a : A)

end
```

```leanOutput err1
Invalid mutually inductive types: `B` has 2 parameter(s), but the preceding type `A` has 1

Note: All inductive types declared in the same `mutual` block must have the same parameters
```

构造器不能在另一个 inductive type 构造器的字段里使用（这里其实嵌套了 {lean empty}`Eq`）：

```lean e -keep +error (name := err2)
mutual

inductive A where
  | mk (b : B)

inductive B where
  | mk {b : B} (a : A) (h : a = A.mk b)

end
```

```leanOutput err2
Invalid field notation: Field projection operates on types of the form `C ...` where C is a constant. The expression
  A
has type `Sort ?u.4` which does not have the necessary form.
```

```leanOutput err2
(kernel) invalid nested inductive datatype 'Eq', nested inductive datatypes parameters cannot contain local variables.
```

Mutual block 的处理是在 kernel 中发生的，elaborator 会把整个 block 发送到 kernel
让它检查，检查过后 kernel 而*不是* elaborator 会生成 recursor。

# Nested Inductive Types

Nested 顾名思义，就是一个 inductive type 构造器的字段中这个类型被嵌套进了另一个 inductive type。

一个简单的例子：

```leanInit e
```

```lean e
inductive I where
  | mk (i : Option I)
```

这里 `I` 就被嵌套进了另一个 inductive type，{lean empty}`Option` 之中。

另一个很经典的：

```lean e
inductive Tree (α : Type) where
  | leaf (value : α) : Tree α
  | node (children : List (Tree α)) : Tree α
```

来看看 inductive type 最重要的东西——它的 recursor：

```lean e (name := Tree.rec)
#print Tree.rec
```

```leanOutput Tree.rec
recursor Tree.rec.{u} : {α : Type} →
  {motive_1 : Tree α → Sort u} →
    {motive_2 : List (Tree α) → Sort u} →
      ((value : α) → motive_1 (Tree.leaf value)) →
        ((children : List (Tree α)) → motive_2 children → motive_1 (Tree.node children)) →
          motive_2 [] →
            ((head : Tree α) → (tail : List (Tree α)) → motive_1 head → motive_2 tail → motive_2 (head :: tail)) →
              (t : Tree α) → motive_1 t
number of parameters: 1
number of indices: 0
number of motives: 2
number of minors: 4
rules:
for Tree.leaf (1 fields): fun α motive_1 motive_2 leaf node nil cons value => leaf value
for Tree.node (1 fields): fun α motive_1 motive_2 leaf node nil cons children =>
  node children (Tree.rec_1 leaf node nil cons children)
```

这里有两个 motive 分别对应正常的 `Tree α` 以及嵌套的 `List (Tree α)`。
从计算规则能看到，它把嵌套时的操作代理给了一个 _辅助_ recursor：

```lean e (name := Tree.rec_1)
#print Tree.rec_1
```

```leanOutput Tree.rec_1
recursor Tree.rec_1.{u} : {α : Type} →
  {motive_1 : Tree α → Sort u} →
    {motive_2 : List (Tree α) → Sort u} →
      ((value : α) → motive_1 (Tree.leaf value)) →
        ((children : List (Tree α)) → motive_2 children → motive_1 (Tree.node children)) →
          motive_2 [] →
            ((head : Tree α) → (tail : List (Tree α)) → motive_1 head → motive_2 tail → motive_2 (head :: tail)) →
              (t : List (Tree α)) → motive_2 t
number of parameters: 1
number of indices: 0
number of motives: 2
number of minors: 4
rules:
for List.nil (0 fields): fun α motive_1 motive_2 leaf node nil cons => nil
for List.cons (2 fields): fun α motive_1 motive_2 leaf node nil cons head tail =>
  cons head tail (Tree.rec leaf node nil cons head) (Tree.rec_1 leaf node nil cons tail)
```

对比一下它们就会发现除了 major premise 以及最后的 conclusion 外它们形状是完全一致的。
此外，它们还是相互递归的。

这个细节来自于目前 nested inductive types 的实现：kernel 在临时环境中把它们 _翻译_ 成消去嵌套、
只有相互递归 mutual block 中的辅助类型，
对着辅助类型生成 recursor 之后再把其中提到辅助类型的地方重命名回来。
最后扔掉辅助类型定义，只保留辅助 recursor 添加到原来的类型上。

具体来说这个例子手动翻译一下就是：


```lean e
mutual

inductive _nested.Tree (α : Type) where
  | leaf (valie : α) : _nested.Tree α
  | node (children : _nested.Tree α) : _nested.Tree α

inductive _nested.List_1 (α : Type) where
  | nil : _nested.List_1 α
  | cons (head : _nested.Tree α)  (tail : _nested.List_1 α) : _nested.List_1 α

end
```

`Tree` 定义中发生的嵌套字段是 `node` 构造器中的 `List (Tree α)`。这里 `Tree α` 是 `List` 的 inductive parameter。
所以创建的辅助类型 `_nested.List_1` 就是特化的 `List`——`List (Tree α)`，其包含 `List` 的所有构造器
并把 `α` 替换为 `_nested.Tree`。

我们来对比一下手动翻译之后 `_nested.Tree.rec` 的 recursor 和之前的 `Tree.rec` ：

```lean e (name := nestedTree.rec)
#print _nested.Tree.rec
```

```leanOutput nestedTree.rec
recursor _nested.Tree.rec.{u} : {α : Type} →
  {motive_1 : _nested.Tree α → Sort u} →
    {motive_2 : _nested.List_1 α → Sort u} →
      ((valie : α) → motive_1 (_nested.Tree.leaf valie)) →
        ((children : _nested.Tree α) → motive_1 children → motive_1 children.node) →
          motive_2 _nested.List_1.nil →
            ((head : _nested.Tree α) →
                (tail : _nested.List_1 α) → motive_1 head → motive_2 tail → motive_2 (_nested.List_1.cons head tail)) →
              (t : _nested.Tree α) → motive_1 t
number of parameters: 1
number of indices: 0
number of motives: 2
number of minors: 4
rules:
for _nested.Tree.leaf (1 fields): fun α motive_1 motive_2 leaf node nil cons valie => leaf valie
for _nested.Tree.node (1 fields): fun α motive_1 motive_2 leaf node nil cons children =>
  node children (_nested.Tree.rec leaf node nil cons children)
```

```leanOutput Tree.rec
recursor Tree.rec.{u} : {α : Type} →
  {motive_1 : Tree α → Sort u} →
    {motive_2 : List (Tree α) → Sort u} →
      ((value : α) → motive_1 (Tree.leaf value)) →
        ((children : List (Tree α)) → motive_2 children → motive_1 (Tree.node children)) →
          motive_2 [] →
            ((head : Tree α) → (tail : List (Tree α)) → motive_1 head → motive_2 tail → motive_2 (head :: tail)) →
              (t : Tree α) → motive_1 t
number of parameters: 1
number of indices: 0
number of motives: 2
number of minors: 4
rules:
for Tree.leaf (1 fields): fun α motive_1 motive_2 leaf node nil cons value => leaf value
for Tree.node (1 fields): fun α motive_1 motive_2 leaf node nil cons children =>
  node children (Tree.rec_1 leaf node nil cons children)
```

以及辅助类型 `_nested.List_1` 的 recursor 和 `Tree` 的辅助 recursor：

```lean e (name := nested.List_1.rec)
#print _nested.List_1.rec
```

```leanOutput nested.List_1.rec
recursor _nested.List_1.rec.{u} : {α : Type} →
  {motive_1 : _nested.Tree α → Sort u} →
    {motive_2 : _nested.List_1 α → Sort u} →
      ((valie : α) → motive_1 (_nested.Tree.leaf valie)) →
        ((children : _nested.Tree α) → motive_1 children → motive_1 children.node) →
          motive_2 _nested.List_1.nil →
            ((head : _nested.Tree α) →
                (tail : _nested.List_1 α) → motive_1 head → motive_2 tail → motive_2 (_nested.List_1.cons head tail)) →
              (t : _nested.List_1 α) → motive_2 t
number of parameters: 1
number of indices: 0
number of motives: 2
number of minors: 4
rules:
for _nested.List_1.nil (0 fields): fun α motive_1 motive_2 leaf node nil cons => nil
for _nested.List_1.cons (2 fields): fun α motive_1 motive_2 leaf node nil cons head tail =>
  cons head tail (_nested.Tree.rec leaf node nil cons head) (_nested.List_1.rec leaf node nil cons tail)
```

```leanOutput Tree.rec_1
recursor Tree.rec_1.{u} : {α : Type} →
  {motive_1 : Tree α → Sort u} →
    {motive_2 : List (Tree α) → Sort u} →
      ((value : α) → motive_1 (Tree.leaf value)) →
        ((children : List (Tree α)) → motive_2 children → motive_1 (Tree.node children)) →
          motive_2 [] →
            ((head : Tree α) → (tail : List (Tree α)) → motive_1 head → motive_2 tail → motive_2 (head :: tail)) →
              (t : List (Tree α)) → motive_2 t
number of parameters: 1
number of indices: 0
number of motives: 2
number of minors: 4
rules:
for List.nil (0 fields): fun α motive_1 motive_2 leaf node nil cons => nil
for List.cons (2 fields): fun α motive_1 motive_2 leaf node nil cons head tail =>
  cons head tail (Tree.rec leaf node nil cons head) (Tree.rec_1 leaf node nil cons tail)
```

它们几乎是完全能对应上的——并且所有 `_nested.List_1 α` 都被还原回了 `List (Tree α)`。
辅助类型的 recursor 成了原来类型的辅助 recursor，其中特化的辅助类型被替换回了原来类型中发生嵌套的那个 inductive type 应用到
原来类型上。

# 翻译算法

看过具体的例子之后翻译过程其实就不难理解了。考虑以下 inductive type，
有 {typst}`[r]` 个构造器；每个构造器只有一个字段，嵌套了 {typst}`[d_i]` 个 inductive types：

```
inductive T where
  | c₁ : F₁,₁ (F₁,₂ (... (F₁,d₁ T) ...)) → T
  | c₂ : F₂,₁ (F₂,₂ (... (F₂,d₂ T) ...)) → T
  | ...
  | cᵣ : Fᵣ,₁ (Fᵣ,₂ (... (Fᵣ,dᵣ T) ...)) → T
```

Kernel 维护了一个 work queue 用于计算嵌套翻译的闭包。最初队列只有 {typst}`T` 本身：

{typst}`[T]`

处理 {typst}`T`：对于其所有构造器发生嵌套类型 uniquely 生成辅助 inductive types （特化其 parameters 为 `T`）并加入队列：

{typst +display}`[T, A_(1,1), A_(2,1), ..., A_(r, 1)]`

注意到一次处理只会剥开一层。现在该处理 {typst}`A_(1,1)` 了。
类似地，我们创造另一个辅助类型 {typst}`A_(1,2)` 并从 {typst}`F_(1,1)` 拷贝所有的构造器。

更新队列之后：

{typst +display}`[T, A_(1,1), A_(2,1), ..., A_(r, 1), A_(1,2)]`

沿着队列处理下去直到 {typst}`A_(r,1)`，我们有：

{typst +display}`[T, A_(1,1), A_(2,1), ..., A_(r, 1), A_(1,2), A_(2,2), ..., A_(r, 2)]`

接着处理到 {typst}`A_(1,2)` 时发现它依旧存在嵌套，那么继续处理直到队尾，
就这样一点一点剥开并特化嵌套得到（每一行是一次前进的结果）：

{typst +display}`
mat(delim: "[",
  T;
  A_(1,1), A_(2,1), ..., A_(r, 1);
  A_(1,2), A_(2,2), ..., A_(r, 2);
  ...;
  A_(1, d_1), A_(2, d_2), ..., A_(r, d_r)
 )
`

比较重要的一点是同样的特化会被 memoized，例如：

```lean e
inductive T where
  | c₁ : List (Option (List T)) → T
  | c₂ : Option (Option (List T)) → T
```

中 `Option T` 和 `Option (List T)`并不会被处理多次，大概是：

```
[ T, ListOptionListT, OptionOptionListT ]
```

```
[ T, ListOptionListT, OptionOptionListT,
  OptionListT
]
```

```
[ T, ListOptionListT, OptionOptionListT,
  OptionListT,
  ListT
]
```

# 被忽略的检查

现在我们可以看下 [#14576](https://github.com/leanprover/lean4/issues/14576) 到底是怎么个事。
因为 bug 在当前版本已经被修了：

```lean e (name := ver)
#eval Lean.versionString
```

```leanOutput ver
"4.31.0"
```

我们找一个老一点的 Lean 版本。
Verso 支持这样加载不同版本的子项目，但是从其他项目产生 diagnostics 就没有 info tree 了，样式也会变得不好看。

{leanExampleProject examples "Subprojects/NestedInductives"}

{leanCommand examples version}

```leanOutput version
"4.29.0"
```

现在我们定义 `Foo`，它有两个 inductive parameters:

{leanCommand examples Foo}

接下来我们尝试定义嵌套了 `Foo` 的 `Bar`：

```lean e -show
inductive Foo (b : Bool) (α : Type) where
  | mk (x : α) : Foo b α
```

```lean e +error
inductive Bar where
  | mk : Foo 1 Bar → Bar
```

显然这东西不应该通过类型检查——`Foo` 的类型是：

```lean e (name := FooT)
#check Foo
```

```leanOutput FooT
Foo (b : Bool) (α : Type) : Type
```

即 `Foo : Bool → Type → Type`，而我们在尝试把 `1` 传给一个 {lean empty}`Bool` 参数。

为了绕过 elaborator，我们用一些简单的元编程直接构造并定义它：

{leanCommand examples bug}

{leanCommand examples Bar}

`#bug` 执行成功——太坏了，这个东西通过了 kernel 的检查！
打印一下 `Bar` 的 recursor 我们能清晰地看到 `Foo 1 Bar` 没有产生任何错误：

{leanCommand examples Bar.rec}

```leanOutput Bar.rec (whitespace := lax)
recursor Bar.rec.{u} : {motive_1 : Bar → Sort u} →
  {motive_2 : Foo 1 Bar → Sort u} →
    ((x : Foo 1 Bar) → motive_2 x → motive_1 (Bar.mk x)) →
      ((x : Bar) → motive_1 x → motive_2 (Foo.mk x)) → (t : Bar) → motive_1 t
number of parameters: 0
number of indices: 0
number of motives: 2
number of minors: 2
rules:
for Bar.mk (1 fields): fun motive_1 motive_2 mk mk_1 x =>
  mk x (@Bar.rec_1 motive_1 motive_2 mk mk_1 x)
```

回顾之前的翻译流程，我们可以把辅助类型手动写出来：

```lean empty
mutual

inductive Bar where
  | mk : Foo1Bar → Bar

inductive Foo1Bar where
  | mk (x : Bar) : Foo1Bar

end
```

从翻译结果上来看这一点问题都没有，kernel 就生成 recursor 然后 accept 了。
问题出现在还原：从辅助类型生成了 recursor 后 `Foo1Bar` 会被还原回 `Foo 1 Bar`，
这个过程是没有类型检查的！`Foo 1 Bar` 这个错误的 term 就漏到了 recursor 之中，导致了 soundness bug。
也就是说 kernel 没有意识到一个 malformed 的 inductive type 在经过 nested inductive 翻译之后可能会变得类型正确；
而实际上翻译过程很可能不维持类型的正确性。

# 更多的问题

总之现在 nested inductive types 的翻译就很灵车，存在一些限制以及更多 bug。

## 嵌套识别

翻译只有发生嵌套字段的头部 syntactically 是另一个 inductive type 时才会发生：

```leanInit e
```

```lean e -keep +error (name := abbr)
abbrev List' := List

inductive Tree (α : Type) where
  | leaf : α → Tree α
  | node : List' (Tree α) → Tree α
```

```leanOutput abbr
(kernel) arg #2 of 'Tree.node' contains a non valid occurrence of the datatypes being declared
```

非头部就不一定要求 syntactic 了，这种 `whnf` 之后暴露出的嵌套 kernel 可以接受。
但是 elaborator 生成 {lean empty}`SizeOf` instance 会爆炸（[#9448](https://github.com/leanprover/lean4/pull/9448)）：

```lean e -keep +error (name := size_of)
inductive Tree (α : Type) where
  | leaf : α → Tree α
  | node : List (id (Tree α)) → Tree α
```

```leanOutput size_of
maximum recursion depth has been reached
use `set_option maxRecDepth <num>` to increase limit
use `set_option diagnostics true` to get diagnostic information
```

## 默认值

因为 {lean empty}`optParam` 的存在导致中间 ` _nested.List_1` 没有正确的被翻译回
`List I`，让类型检查失败（[#4824](https://github.com/leanprover/lean4/issues/4824)）。

```lean e -keep +error (name := def)
inductive I where
  | mk (xs : List I := [])
```

```leanOutput def
(kernel) application type mismatch
  optParam _nested.List_1 []
argument has type
  List I
but function has type
  _nested.List_1 → Type
```

## 私有构造器

如果翻译过程中遇到 `private` 构造器翻译会直接失败([#10789](https://github.com/leanprover/lean4/issues/10789))。

我们先定义 inductive type `I` 具有私有的构造器 `mk`：
```lean e
inductive I (α : Type) where
  | private mk (x : α)
```

尽管在同一个文件里我们可以直接写

```lean e (name := I.mk)
#check I.mk
```

```leanOutput I.mk
I.mk {α : Type} (x : α) : I α
```

实际上 Lean 中私有名字是给名字加上一堆前缀实现的：

```lean e (name := I)
#print I
```

```leanOutput I
inductive I : Type → Type
number of parameters: 1
constructors:
_private.Blog.Scribbles.NestedInductive.0.I.mk : {α : Type} → α → I α
```

可以看到 `I.mk` 的真名是一大长串，而我们能直接写 `I.mk` 是因为 name resolution 有特殊处理。


那么我们现在定义 `J`，让它拥有一个嵌套 `I` 的字段：

```lean e +error (name := private)
inductive J where
  | mk (x : I J)
```

```leanOutput private
(kernel) constant has already been declared '_private.Blog.Scribbles.NestedInductive.0.I.mk'
```

乍一看这个错误意义不明，还把 `I.mk` 的全名漏出来了。

具体发生的是：这里我们需要把 `I.mk` 改成 `I'.mk`——strip 掉前缀 `I` 然后加上 `I'`，
但是因为私有定义现在 `I.mk` 的 全名是 `_private.Blog.Scribbles.NestedInductive.0.I.mk`，所以重命名操作就失败了。
Kernel 接着用这个名字去定义辅助类型，自然就得到了“constant has already been declared”错误。

本质上这个和上面默认值是类似问题导致的：辅助定义的名字没有被正确地翻译过去。
在翻译过程中把辅助类型构造器翻译过去、类型检查、再翻译回来的实现隐式地要求了原构造器名字*必须*以原 inductive type 开头。
有了这些私有名字的前缀之后这个 invariant 就被破坏了，所以翻译第一步中的改名字就失败了。

我感觉不太能 exploit 这个 bug 弄出 soundness bug 来，因为一上来定义就失败了，不过欢迎有想法的读者在下方留言……
