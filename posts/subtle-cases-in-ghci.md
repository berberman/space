---
title: Subtle cases in GHCi
author: berberman
date: 2021-04-11 23:58:00
---

我写这篇文章的原因是无意中发现 `head []` 在关闭 `ExtendedDefaultRules` 的 GHCi 中可以通过类型检查，让我感到很意外；经过一番摸索之后——

## 前言

GHCi，即 GHC 的交互式（*interactive*）环境，可以用来求值 Haskell 表达式、加载编译好的 Haskell 程序、交互式推断表达式的类型等等。想必使用 GHCi 是每一位 Haskell 程序员的必备技能。然而，在 GHCi 中类型检查规则与默认情况下的 GHC 略有不同，这可能在某种程度上会带来一定困惑。在这篇文章中，我们将一起认识带来这个不同的语言扩展，并通过 GHC 以及 GHCi 的源码了解语句在 GHCi 中是怎样被执行、并打印的。

## GHCi 中默认启用的语言扩展

一些在 GHC 中无法通过类型检查的表达式在 GHCi 环境中却可以通过，我们来看一个最简单的例子：

```haskell
-- 在 ghci 中
λ> show $ reverse []
"[]"

-- 在普通的 ghc 中
foo = show $ reverse []
-- • Ambiguous type variable ‘a0’ arising from a use of ‘show’
--   prevents the constraint ‘(Show a0)’ from being solved.
```

 在 GHCi 中，`show $ reverse []` 返回了一个字符串 `"[]"`，但在 GHC 中这个表达式不能过编译——正如错误信息所说，`Show a` 约束中的 `a` 是 *不确定* 的，编译器无法从 `[]` 中推断出来，于是就不知道该选择哪个实例的 `show` 函数了。听起来很有说服力，因为 `[a]` 中 `a` 的选择可能会改变 `[a]` 的显示方法：

```haskell
data X = X
  deriving Show

instance {-# OVERLAPS #-} Show [X] where
  show [] = "??"
  show [x] = show x
  show (x:xs) = show x ++ ", " ++ show xs

-- >>> show ([] :: [X])
-- "??"

-- >>> show [X, X, X]
-- "X, X, X"

-- >>> show ([] :: String)
-- "\"\""
```

看来在 GHCi 中有特殊操作，帮助我们选择了一个 `a` 来显示空列表。让我们看看 GHCi 默认启用了哪些扩展：

```haskell
λ> :showi language
with the following modifiers:
  -XNoDatatypeContexts
  -XExtendedDefaultRules
  -XNoMonomorphismRestriction
  -XNondecreasingIndentation
```

### DatatypeContexts

`NoDatatypeContexts`…… 跑一下题，不得不说 `DatatypeContexts` 是个彻底失败的特性，来看个例子：

```haskell
newtype Eq a => B a = B a
```

我们想让用来构造 `B` 的 `a` 必须满足 `Eq` 约束，好，咱来写个函数体验一下：

```haskell
bEq :: B a -> B a -> Bool
bEq (B x) (B y) = x == y

-- • No instance for (Eq a) arising from a use of ‘B’
--   Possible fix:
--     add (Eq a) to the context of
--       the type signature for:
--         bEq :: forall a. B a -> B a -> Bool
-- • In the pattern: B x
--   In an equation for ‘bEq’: bEq (B x) (B y) = x == y
```

非常不幸，这是个伤星的故事——在模式匹配解构 `B` 时，它不光没有附带 `a` 满足 `Eq` 的证据，反倒向我们索要这个证据。在类型构造器前加的 `Eq a =>` 只确保了在 **构造** `B` 时 `a` 必须满足 `Eq`。于是我们需要在所有用到 `B` 的函数的签名上都加上 `Eq a =>`（如果不这样我们是没有办法解构 `B` 的）显然非常离谱。当然这个语言扩展已经将近废弃十年了，新的入门教程中几乎不会再出现它的身影，一些稍老的材料中也会提醒读者不要使用它。

### ExtendedDefaultRules

能让开头 `show $ reverse []` 在 GHCi 工作的正是这个语言扩展。这个扩展意为“扩展默认的规则”，那我们就得先搞明白“默认的规则”是啥。首先我们要知道，Haskell 中整数字面值的类型是 `Num a => a`，浮点数字面值的类型是 `Fractional a => a`。那么问题来了：`233 == 233` 中，`233` 的类型是啥？`(==) :: Eq a => a -> a -> Bool`，这和前面例子中的 `Show` 很相似——光有 `Num a` 约束我们是无法进行比较的，除非 `Num a => a` 被实例化成某个 *具体* 的类型。这里则有 `233 :: Integer`。为什么呢？在 Haskell 2010 Report 中规定：

* 表达式 *e* 若有类型 *forall U. cx => t*，该类型的全称量化 *U* 中含有类型变量 *u*，*u* 在 *cx* 中出现了但却没在 *t* 中出现，我们就说这个类型是 *非法* 的，而表达式 *e* 的类型是 *不确定* 的
* 在模块顶层可以使用 *default (t1 , … , tn)  (n >= 0)* 为该模块声明一个 *默认规则*
* 当遇到 *不确定* 类型时，假设类型变量 *v* 是 *不确定*  的，在以下条件满足时我们说 *v* 是 *可默认的*：
  * *v* 仅出现在一个约束 *C v* 中（*C* 是一个类型类）
  * 约束的类型类必须是 `Num` 或 `Num` 的子类
  * 这些类型类必须定义在 Prelude 中
* *可默认的* 类型变量会被 *默认规则* 列表中第一个所满足约束的类型替代，如果没有这样的类型则产生错误
* 每个模块仅能有一个 *默认规则* 声明，它的作用域是该模块；如果一个模块没有 *默认规则* 声明，那么使用 `default (Integer, Double)`
* 为模块声明 `default ()` 会禁用掉这个功能

不难理解 `print 233`、`233 == 233` 这些时候都有 `233 :: Integer`。那么开启 `ExtendedDefaultRules` 之后会发生甚么事呢？根据 GHCi 文档，当遇到约束 *(C1 a, C2 a, ..., Cn a)* 时：

1. 找到所有未解析的约束
2. 以 *C a* 这样的形式，把未解析的约束进行分组，使得一个组内不同的 *C* 共享相同的 *a*
3. 仅保留包含至少一个 *C* 为 *交互式类型类* 的组
4. 对于每个留下的组 *G*，尝试将定义在 *默认规则* 中的 *ty* **依次**代入 *a*；如果这让 *G* 中所有的约束都能被解析了，那么 *a* 的 *可默认类型* 就是 *ty*

此外，`ExtendedDefaultRules` 还会：

* 定义 `Show`、`Eq`、`Ord`、`Foldable` 或 `Traversable` 为 *交互式类型类*
* 放宽限制——*默认规则* 中的类型必须要实现 `Num` 改为必须是 *交互式类型类* 的实例
* 将标准情况下的 `default (Integer, Double)` 改为 `default ((), [], Integer, Double)`

按照这样的处理方式，只要 *默认规则* 中含有任意满足 `Show` 约束的类型，咱叫它 `Foo`，`show $ reverse []` 就可以通过类型检查——列表的类型会被推断成 `[Foo]`。

### MonomorphismRestriction

> 这是个比较常见的坑，而且把每种情况解释一遍会很占篇幅，因此在这儿就说个大概。

上节我们提到过，`print 233` 中受到 *默认规则*  的影响  `233` 的类型是 `Integer`。那么在一个整数字面值的绑定中呢？

```haskell
qwq = 233
-- Top-level binding with no type signature: qwq :: Integer
```

GHC 告诉我们 `qwq` 的类型是 `Integer`。可这里并没有涉及到使用约束，为什么字面值还是被实例化成了一个 *具体* 的类型呢？这就是 `MonomorphismRestriction` 的作用，没有类型签名的绑定会可能被应用 *默认规则*。这会让我们创建的绑定没有那么地“多态”。让我们一起看几个来自 Wiki 的例子：

```haskell
f1 x = show x

f2 = \x -> show x

f3 :: (Show a) => a -> String
f3 = \x -> show x

f4 = show

f5 :: (Show a) => a -> String
f5 = show
```

默认不动任何扩展的情况下，`f1`、`f3` 以及 `f5` 都是我们想要的，它们具有类型 `Show a => a -> String`；而 `f2` 和 `f4` 无法通过类型检查，`Show a` 中的 `a` 是 *不确定* 的。更有意思的是，这时候要是启用 `ExtendedDefaultRules`，`f2` 和 `f4` 也是良型的了，但它们的类型是 `() -> String`。原因很简单，上节中我们提到过，`()` 会被插入到 *默认规则* 的头部，所以 GHC 选择了第一个能解析约束的 `()` 来实例化 `a`。当然，这有些反直觉——eta-reduce 一下居然改变了语义。根据 Haskell 2010 Report，这种限制是为了解决两个问题：无法通过标注类型签名解决的不确定性，以及不必要的重复求值。在 GHCi 中，我们通常不希望这种单态化发生：

```haskell
λ> :set -XMonomorphismRestriction
λ> plus = (+)
λ> plus 233.3 3

<interactive>:28:6-10: error:
    • No instance for (Fractional Integer)
        arising from the literal ‘233.3’
    • In the first argument of ‘plus’, namely ‘233.3’
      In the expression: plus 233.3 3
      In an equation for ‘it’: it = plus 233.3 3
```

显然，这里 `plus` 的类型是 `Integer -> Integer -> Integer`。但在文件中用 GHC 编译是不会有问题的：

```haskell
plus = (+)

qwq = plus 233.3 3
```

因为 GHC 会在推断出 `plus` 的类型前先看一看它被调用的地方，这时就有 `plus :: Double -> Double -> Double`；但在 GHCi 中语句是一步一步执行的，声明完 `plus` 直接就定下来了。

### NondecreasingIndentation

咱也不知道这是干啥的，查了下好像在某些情况下可以让缩进往前越一级（？）不懂在 GHCi 里有啥用（x

## GHCi 中的语句执行

在 GHCi session 中可以：

```haskell
λ> let x = 1          -- 绑定纯的变量
λ> x' = 1             -- 绑定纯的变量，let 可以省略
λ> y <- pure 2        -- 绑定 IO 结果到变量
λ> 3 + 3              -- 求值表达式并打印
6
λ> print 4            -- 执行 IO 操作
4
λ> it                 -- 获得上次的求值结果
()
```

是不是感觉有点像在一个 `IO ()` do notation 中，但可以省略 let、还多了个 `it`。这个 `it` 比较有意思，咱后面细说。咱可以直接看 GHCi 中的某个入口函数，它直接接收输入的字符串：

```haskell
-- GHCi/UI.hs

runStmt :: GhciMonad m => String -> SingleStep -> m (Maybe GHC.ExecResult)
runStmt input step = do
  pflags <- initParserOpts <$> GHC.getInteractiveDynFlags
  st <- getGHCiState
  let source = progname st
  let line = line_number st

  -- 如果输入是一个 statement
  if | GHC.isStmt pflags input -> do
         hsc_env <- GHC.getSession
         -- 尝试 parse 它
         mb_stmt <- liftIO (runInteractiveHsc hsc_env (hscParseStmtWithLocation source line input))
         case mb_stmt of
           Nothing ->
             -- parse 失败什么也不做
             return (Just exec_complete)
           Just stmt ->
             -- 调用 run_stmt 执行
             run_stmt stmt
     -- 如果输入是一个 import
     | GHC.isImport pflags input -> do
         -- 添加 import
         addImportToContext input
         return (Just exec_complete)

     -- 其他情况我们把输入当作 declaration
     | otherwise -> do
         hsc_env <- GHC.getSession
         let !ic = hsc_IC hsc_env
         -- 尝试 parse 成 declaration
         decls <- liftIO (hscParseDeclsWithLocation hsc_env source line input)
         -- 看下个代码块
         run_decls decls
```

`x = y` 是一个声明（*declaration*），但 `let x = y` 是一个语句（*statement*)。GHCi 的处理非常直接，把所有这种声明 parse 完之后将 AST 改写成 let 语句并执行：

```haskell
-- GHCi/UI.hs

run_decls :: GhciMonad m => [LHsDecl GhcPs] -> m (Maybe GHC.ExecResult)
run_decls [L l (ValD _ bind@FunBind{})] =
  run_stmt (mk_stmt (locA l) bind) -- 调用 run_stmt 执行
run_decls [L l (ValD _ bind@VarBind{})] =
  run_stmt (mk_stmt (locA l) bind) -- 调用 run_stmt 执行
run_decls decls = do -- 如果不是 FunBind 或者 VarBind，还得按声明来处理
  m_result <- GhciMonad.runDecls' decls
  forM m_result $ \result ->
    afterRunStmt (const True) (GHC.ExecComplete (Right result) 0)

-- 把 Bind 变成 LetStmt
mk_stmt :: SrcSpan -> HsBind GhcPs -> GhciLStmt GhcPs
mk_stmt loc bind = let la = ... in la (LetStmt noAnn (HsValBinds noAnn (ValBinds NoAnnSortKey (unitBag (la' bind)) [])))
```

可以看到“省略 let 的绑定”是在 GHCi 入口处实现的。那么 `run_stmt` 是啥？GHCi 最终调用的函数应该是 [`execStmt'`](https://hackage.haskell.org/package/ghc-8.10.2/docs/GHC.html#v:execStmt-39-) ——这是 GHC 暴露 API 的一部分，所以有 Haddock 文档。这个函数干的最重要的一件事就是调用了 [`hscParsedStmt`](https://hackage.haskell.org/package/ghc-8.10.2/docs/HscMain.html#v:hscParsedStmt)：而语句在这个函数中走过了完整的编译过程：[`tcRnStmt`](https://hackage.haskell.org/package/ghc-8.10.2/docs/TcRnDriver.html#v:tcRnStmt) 完成 rename & typecheck、[`deSugarExpr`](https://hackage.haskell.org/package/ghc-8.10.2/docs/Desugar.html#v:deSugarExpr) 完成 desugar & generate core、[`hscCompileCoreExpr`](https://hackage.haskell.org/package/ghc-8.10.2/docs/HscMain.html#v:hscCompileCoreExpr) 完成 codegen & link。我们只关心类型检查部分。从注释我们可以清晰地知道 GHC 在类型检查时加 buff，实现创建交互式绑定以及 `it` 变量的策略：

```haskell
                Typechecking Stmts in GHCi

Here is the grand plan, implemented in tcUserStmt

        What you type                   The IO [HValue] that hscStmt returns
        -------------                   ------------------------------------
        let pat = expr          ==>     let pat = expr in return [coerce HVal x, coerce HVal y, ...]
                                        bindings: [x,y,...]

        pat <- expr             ==>     expr >>= \ pat -> return [coerce HVal x, coerce HVal y, ...]
                                        bindings: [x,y,...]

        expr (of IO type)       ==>     expr >>= \ it -> return [coerce HVal it]
          [NB: result not printed]      bindings: [it]

        expr (of non-IO type,   ==>     let it = expr in print it >> return [coerce HVal it]
          result showable)              bindings: [it]

        expr (of non-IO type,
          result not showable)  ==>     error

```

`HValue` 相当于一个装着 `Any` 的容器——编译、求值一个表达式的结果可能有任意类型。GHC 将这几种情况分成三类，名曰 plans：

* Plan A. `[it <- e; print e]`（`it` 不能是 `()`）
* Plan B. `[it <- e]`
* Plan C. `[let it = e; print it]`

可在 `tcUserStmt` 中找到相应的代码：

```haskell
-- TcRnDriver.hs

tcUserStmt :: GhciLStmt GhcPs -> TcM (PlanResult, FixityEnv)
tcUserStmt (dL->L loc (BodyStmt _ expr _ _))
  = do { (rn_expr, fvs) <- checkNoErrs (rnLExpr expr)
-- ...省略
          let
              -- [it = expr]
              the_bind  = cL loc $ (mkTopFunBind FromSource
                                     (cL loc fresh_it) matches)
                                         { fun_ext = fvs }

              -- [let it = expr]
              let_stmt  = cL loc $ LetStmt noExtField $ noLoc $ HsValBinds noExtField
                           $ XValBindsLR
                               (NValBinds [(NonRecursive,unitBag the_bind)] [])

              -- [it <- e]
              bind_stmt = cL loc $ BindStmt noExtField
                                       (cL loc (VarPat noExtField (cL loc fresh_it)))
                                       (nlHsApp ghciStep rn_expr)
                                       (mkRnSyntaxExpr bindIOName)
                                       noSyntaxExpr

              -- [; print it]
              print_it  = cL loc $ BodyStmt noExtField
                                           (nlHsApp (nlHsVar interPrintName)
                                           (nlHsVar fresh_it))
                                           (mkRnSyntaxExpr thenIOName)
                                                  noSyntaxExpr

              it_plans = [
                    -- Plan A
                    do { stuff@([it_id], _) <- tcGhciStmts [bind_stmt, print_it]
                       ; it_ty <- zonkTcType (idType it_id)
                       ; when (isUnitTy $ it_ty) failM -- it 不能是 ()
                       ; return stuff },

                        -- Plan B
                    tcGhciStmts [bind_stmt],

                        -- Plan C
                        -- 先看一看 let 绑定是不是良型的
                        -- 否则会得到两个错误信息，一个在 let 绑定上，一个在打印上
                    do { _ <- checkNoErrs (tcGhciStmts [let_stmt])
                       ; tcGhciStmts [let_stmt, print_it] } ]

-- ...省略
```

GHC 在这里加的特效大概就是拼接 renamed AST 来进行下一步的 `tcGhciStmts`：

* Plan A：`e` 是一个 `IO` 操作，并且这个操作的返回值可以打印并且不是 `()`。将 `e` 的执行结果绑定到 `it` 上，并打印 `it`
* Plan B：`e` 是一个 `IO` 操作，并且这个操作的返回值无法打印。将 `e` 的执行结果绑定到 `it` 上，不打印
* Plan C：`e` 是一个可以打印的表达式。将 `e` 绑定到 `it` 上，并打印 `it`

从 Plan A 开始依次尝试，若全部失败就打印错误信息。这里的“打印”并不是使用 `print`，而是 `ic_int_print `。后者是一个任意具有类型 `a -> IO ()` 的函数，可以通过 `-interactive-print=<FUNCTION_NAME>` 选项在 GHCi 启动时指定。

## 总结

到目前为止，相信读者已经对 GHCi 这套操作很熟悉了。那么回过头来看我在文章开头提出的困惑：

```haskell
λ> :set -XNoExtendedDefaultRules
λ> head []
*** Exception: Prelude.head: empty list
```

为什么关闭了 `ExtendedDefaultRules` 之后 `head []` 依然能在 GHCi 中预期执行，没有产生 `a` 不确定的错误呢？再看一眼上面的三个 plans —— Plan A 和 Plan C 都需要 `Show` 约束，只有 Plan B 不需要。答案呼之欲出：我们在尝试获取并**执行**（并非打印）一个 `[IO a]` 列表的头部，但这个列表是空的，所以扔出了错误，给我们造成了“正在尝试打印空列表头部”的假象。读完这篇文章可能会获得一些没什么用的知识：

* 像 `head`、`last` 这种 `[a] -> a` 的函数在 GHCi 中应用到空列表时没有用到 `Show` 约束
* `pure ()` 在 GHCi 中不会打印 `()`；`pure 233` 会打印 `233`
* ……

 
