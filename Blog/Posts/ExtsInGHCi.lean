import VersoBlog
import Blog.Categories
import Blog.Meta
open Verso Genre Blog

#doc (Post) "Default Language Extensions Enabled in GHCi" =>

%%%
authors := ["berberman"]
date := {year := 2021, month := 4, day := 11}
categories := [Category.haskell]
%%%

I wrote this post because I stumbled upon the fact that `head []` passes type checking in GHCi even with `ExtendedDefaultRules` disabled.
This surprised me. After some investigation—

# Foreword

GHCi, the _interactive_ environment for GHC, can be used to evaluate Haskell expressions, load compiled Haskell programs, interactively infer types, and more.
Proficiency with GHCi is likely an essential skill for every Haskell programmer.
However, the type-checking rules in GHCi differ slightly from those in standard GHC, which can cause some confusion.
In this post, we will look at the language extensions responsible for these differences and explore the source code of GHC and GHCi to understand how statements are executed and printed.

# Default Language Extensions in GHCi


Some expressions that fail type checking in GHC pass in the GHCi environment. Let's look at the simplest example:

```haskell
-- In ghci
λ> show $ reverse []
"[]"

-- In standard ghc
foo = show $ reverse []
-- • Ambiguous type variable ‘a0’ arising from a use of ‘show’
--   prevents the constraint ‘(Show a0)’ from being solved.
```

In GHCi, `show $ reverse []` returns the string `"[]"`, but in GHC, this expression fails to compile.
As the error message suggests, the `a` in the `Show a` constraint is ambiguous;
the compiler cannot infer it from `[]`, so it doesn't know which instance's show function to choose.
This sounds convincing because the choice of `a` in `[a]` might change how `[a]` is displayed:

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

It seems there is some special handling in GHCi that helps us select an a to display empty lists. Let's see which extensions GHCi enables by default:

```haskell
λ> :showi language
with the following modifiers:
  -XNoDatatypeContexts
  -XExtendedDefaultRules
  -XNoMonomorphismRestriction
  -XNondecreasingIndentation
```

## DatatypeContexts

`NoDatatypeContexts`... To digress a bit, I have to say `DatatypeContexts` turns out to be a completely failed feature. Let's look at an example:

```haskell
newtype Eq a => B a = B a
```

We want the a used to construct `B` to satisfy the `Eq` constraint. Okay, let's write a function to try it out:

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

Unfortunately, this is a sad story—when pattern matching to deconstruct `B`, not only does it not provide evidence that `a` satisfies `Eq`,
it actually demands that evidence from us. The `Eq a =>` before the type constructor only ensures that a must satisfy `Eq` when constructing `B`.
Consequently, we would need to add `Eq a =>` to the signatures of all functions using `B` (otherwise we can't deconstruct it), which is obviously absurd.
Of course, this language extension has been effectively deprecated for nearly a decade; it rarely appears in new tutorials, and older materials often warn readers against using it.

## ExtendedDefaultRules

This is the language extension that allows `show $ reverse []` to work in GHCi.
As the name suggests, it "extends the default rules," so we first need to understand what the "default rules" are.
First, note that integer literals in Haskell have the type `Num a => a`, and floating-point literals are `Fractional a => a`.
Now, in `233 == 233`, what is the type of `233`?
Note that `(==) :: Eq a => a -> a -> Bool`. This is similar to `Show` earlier—we can't compare with just a `Num` constraint unless `Num a => a` is instantiated to a concrete type.
Here, we get `233 :: Integer`. Why? The Haskell 2010 Report specifies:

* If an expression _e_ has type _forall U. cx => t_, and the universally quantified _U_ contains a type variable _u_ that appears in _cx_ but not in _t_, we say this type is _ambiguous_, and the type of expression _e_ is _ambiguous_.
* A default rule can be declared for a module at the top level using default _(t₁ , … , tₙ) (n ≥ 0)_.
* When encountering an _ambiguous_ type, assuming the type variable _v_ is ambiguous, we say _v_ is _defaultable_ if the following conditions are met:
  * _v_ appears in only one constraint _C v_ (where _C_ is a type class).
  * The type class of the constraint must be `Num` or a subclass of `Num`.
  * These type classes must be defined in the Prelude.
* A _defaultable_ type variable is replaced by the first type in the default rules list that satisfies the constraint; if no such type exists, an error is produced.
* Each module can have only one _default rule_ declaration, and its scope is that module; if a module has no _default rule_ declaration, `default (Integer, Double)` is used.
* Declaring `default ()` for a module disables this feature.

It's easy to understand that in cases like `print 233` or `233 == 233`, we have `233 :: Integer`. So what happens when `ExtendedDefaultRules` is enabled?
According to the GHCi documentation, when encountering constraints _(C₁ a, C₂ a, ..., Cₙ a)_:

1. Find all unresolved constraints.
2. Group the unresolved constraints in the form _C a_, such that different _C_s in a group share the same a.
3. Keep only the groups containing at least one C that is an _interactive class_.
4. For each remaining group _G_, try to substitute _a_ with the _ty_ defined in the _default rules_ *in order*;
   if this allows all constraints in _G_ to be resolved, then the _defaultable_ type for _a_ is _ty_.

Additionally, `ExtendedDefaultRules`:

* Defines `Show`, `Eq`, `Ord`, `Foldable`, or `Traversable` as _interactive_ classes.
* Relaxes the restriction—types in the _default rules_ must implement `Num` is changed to must be instances of an _interactive class_.
* Changes the standard `default (Integer, Double)` to `default ((), [], Integer, Double)`.

With this handling, as long as the _default rules_ contain any type satisfying the `Show` constraint (let's call it `Foo`), `show $ reverse []` passes type checking—the list type is inferred as `[Foo]`.

## MonomorphismRestriction

> This is a relatively common pitfall, and explaining every case would take too much space, so I'll just give a general overview here.

In the previous section, we mentioned that `233` in `print 233` defaults to `Integer` due to _default rules_. What about in a binding for an integer literal?

```haskell
qwq = 233
-- Top-level binding with no type signature: qwq :: Integer
```

GHC tells us `qwq` is `Integer`. But there's no constraint involved here; why is the literal instantiated to a concrete type?
This is the effect of the `MonomorphismRestriction`: bindings without type signatures may be subject to _default rules_.
This makes our bindings less "polymorphic." Let's look at some examples from the Wiki:

```haskell
f1 x = show x

f2 = \x -> show x

f3 :: (Show a) => a -> String
f3 = \x -> show x

f4 = show

f5 :: (Show a) => a -> String
f5 = show
```

Without touching any extensions, `f1`, `f3`, and `f5` are what we want (they have type `Show a => a -> String`);
`f2` and `f4` fail type checking because the `a` in `Show a` is _ambiguous_.
Interestingly, if `ExtendedDefaultRules` is enabled, `f2` and `f4` become well-typed, but their type is `() -> String`.
The reason is simple: as mentioned in the previous section, `()` is inserted at the head of the _default rules_, so GHC picks the first `()` that resolves the constraint.
This is somewhat counter-intuitive—eta-reduction actually changes semantics.
According to the Haskell 2010 Report, this restriction resolves two issues: ambiguity not solvable by type signatures, and unnecessary re-computation.
In GHCi, we usually don't want this monomorphization to happen:

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

Clearly, `plus` here is `Integer -> Integer -> Integer`. But compiling with GHC in a file works fine:

```haskell
plus = (+)

qwq = plus 233.3 3
```

This is because GHC looks at usage sites before inferring the type of `plus` (getting `plus :: Double -> Double -> Double`);
whereas GHCi executes statement by statement, fixing the type immediately after `plus` is declared.

## NondecreasingIndentation

I don't really know what this is for :P

# Statement Execution in GHCi

In a GHCi session, you can:

```haskell
λ> let x = 1          -- Bind a pure variable
λ> x' = 1             -- Bind a pure variable, let can be omitted
λ> y <- pure 2        -- Bind IO result to a variable
λ> 3 + 3              -- Evaluate expression and print
6
λ> print 4            -- Execute IO operation
4
λ> it                 -- Get result of previous evaluation
()
```

It feels a bit like being inside an `IO ()` do-notation, but you can omit `let`, and there's an extra `it`.
This `it` is interesting; we'll discuss it later. Let's look at an entry function in GHCi that receives the input string:

```haskell
runStmt :: GhciMonad m => String -> SingleStep -> m (Maybe GHC.ExecResult)
runStmt input step = do
  pflags <- initParserOpts <$> GHC.getInteractiveDynFlags
  st <- getGHCiState
  let source = progname st
  let line = line_number st

  -- If input is a statement
  if | GHC.isStmt pflags input -> do
         hsc_env <- GHC.getSession
         -- Try to parse it
         mb_stmt <- liftIO (runInteractiveHsc hsc_env (hscParseStmtWithLocation source line input))
         case mb_stmt of
           Nothing ->
             -- Do nothing if parse fails
             return (Just exec_complete)
           Just stmt ->
             -- Call run_stmt to execute
             run_stmt stmt
     -- If input is an import
     | GHC.isImport pflags input -> do
         -- Add import
         addImportToContext input
         return (Just exec_complete)

     -- Otherwise treat input as a declaration
     | otherwise -> do
         hsc_env <- GHC.getSession
         let !ic = hsc_IC hsc_env
         -- Try to parse as declaration
         decls <- liftIO (hscParseDeclsWithLocation hsc_env source line input)
         -- See next code block
         run_decls decls
```

`x = y` is a declaration, but `let x = y` is a statement. GHCi handles this very directly:
after parsing the declarations, it rewrites the AST into a `let` statement and executes it:

```haskell
-- GHCi/UI.hs

run_decls :: GhciMonad m => [LHsDecl GhcPs] -> m (Maybe GHC.ExecResult)
run_decls [L l (ValD _ bind@FunBind{})] =
  run_stmt (mk_stmt (locA l) bind) -- Call run_stmt to execute
run_decls [L l (ValD _ bind@VarBind{})] =
  run_stmt (mk_stmt (locA l) bind) -- Call run_stmt to execute
run_decls decls = do -- If not FunBind or VarBind, handle as declaration
  m_result <- GhciMonad.runDecls' decls
  forM m_result $ \result ->
    afterRunStmt (const True) (GHC.ExecComplete (Right result) 0)

-- Turn Bind into LetStmt
mk_stmt :: SrcSpan -> HsBind GhcPs -> GhciLStmt GhcPs
mk_stmt loc bind = let la = ... in la (LetStmt noAnn (HsValBinds noAnn (ValBinds NoAnnSortKey (unitBag (la' bind)) [])))
```

We can see that "bindings omitting let" are implemented at the GHCi entry point.
So what is `run_stmt`? GHCi ultimately calls [`execStmt'`](https://hackage.haskell.org/package/ghc-8.10.2/docs/GHC.html#v:execStmt-39-)—this is part of the GHC API, so it has Haddock documentation.
The most important thing this function does is call [`hscParsedStmt`](https://hackage.haskell.org/package/ghc-8.10.2/docs/HscMain.html#v:hscParsedStmt),
where the statement goes through the full compilation process: [`tcRnStmt`](https://hackage.haskell.org/package/ghc-8.10.2/docs/TcRnDriver.html#v:tcRnStmt) (rename & typecheck),
[`deSugarExpr`](https://hackage.haskell.org/package/ghc-8.10.2/docs/Desugar.html#v:deSugarExpr) (desugar & generate core),
and [`hscCompileCoreExpr`](https://hackage.haskell.org/package/ghc-8.10.2/docs/HscMain.html#v:hscCompileCoreExpr) (codegen & link).
We only care about the type checking part. From the comments, we can clearly see the strategy GHC uses during type checking to create interactive bindings and the `it` variable:

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

`HValue` is like a container holding `Any`—the result of compiling and evaluating an expression can be of any type. GHC divides these situations into three categories, called "plans":

* Plan A. `[it <- e; print e]` (`it` cannot be `()`)
* Plan B. `[it <- e]`
* Plan C. `[let it = e; print it]`

The corresponding code can be found in `tcUserStmt`:

```haskell
-- TcRnDriver.hs

tcUserStmt :: GhciLStmt GhcPs -> TcM (PlanResult, FixityEnv)
tcUserStmt (dL->L loc (BodyStmt _ expr _ _))
  = do { (rn_expr, fvs) <- checkNoErrs (rnLExpr expr)
-- ...omitted
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
                       ; when (isUnitTy $ it_ty) failM -- it cannot be ()
                       ; return stuff },

                        -- Plan B
                    tcGhciStmts [bind_stmt],

                        -- Plan C
                        -- First check if the let binding is well-typed
                        -- Otherwise we get two error messages, one on let binding, one on print
                    do { _ <- checkNoErrs (tcGhciStmts [let_stmt])
                       ; tcGhciStmts [let_stmt, print_it] } ]

-- ...omitted
```

The "special effects" GHC adds here are essentially splicing the renamed AST for the next step, `tcGhciStmts`:

* Plan A: `e` is an `IO` operation, and its return value is printable and not `()`. Bind the execution result of `e` to `it`, and print `it`.
* Plan B: `e` is an `IO` operation, and its return value cannot be printed. Bind the execution result of `e` to `it`, do not print.
* Plan C: `e` is a printable expression. Bind `e` to `it`, and print `it`.

It attempts Plan A first; if all fail, it prints an error. The "print" here isn't using `print`, but `ic_int_print`.
The latter is an arbitrary function of type `a -> IO ()` that can be specified via the `-interactive-print=<FUNCTION_NAME>` option when starting GHCi.

# Summary

By now, I trust the reader is familiar with GHCi's operations. Let's look back at the confusion I raised at the beginning:

```haskell
λ> :set -XNoExtendedDefaultRules
λ> head []
*** Exception: Prelude.head: empty list
```

Why does head `[]` still execute as expected in GHCi without producing an ambiguous `a` error after disabling `ExtendedDefaultRules`?
Look at the three plans again—Plan A and Plan C both require the `Show` constraint; only Plan B does not.
The answer is obvious: we are attempting to obtain and *execute* (not print) the head of an `[IO a]` list.
Since the list is empty, it throws an error, giving us the illusion that "it is trying to print the head of an empty list."
Reading this post might provide some useless trivia:

* Functions like `head` and `last` (`[a] -> a`), when applied to empty lists in GHCi, do not use the `Show` constraint.
* `pure ()` in GHCi does not print `()`; but `pure 233` prints `233`.
* ... and some other weird behaviors.
