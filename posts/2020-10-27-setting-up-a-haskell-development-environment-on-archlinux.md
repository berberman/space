---
title: Setting up a Haskell development environment on Arch Linux
author: berberman
---

~~Once you accept the principles of Arch Linux -- being simplicity and modernity -- everything goes easier.~~ In this article, we will use up-to-date Haskell ecosystem by using system provided Haskell packages, getting rid of awkward stack which could eat huge amount of your disk space. We won't going to [nix](https://github.com/Gabriel439/haskell-nix) or [ghcup](https://gitlab.haskell.org/haskell/ghcup-hs), since they are both general Haskell toolchain solutions, not specific to Arch Linux.

## Preface

If you get pandoc, shellcheck, or other Haskell programs installed on your system, you will find that a bunch of packages with `haskell-` prefix emerge frequently when rolling the system, which is pretty verbose and noisy. Thus, many of general users, i.e. not Haskell developers, always complain that "why every time I tries to roll my system, there are so many Haskell packages to be updated, and wait... What Is Haskell?" Remember that Arch Linux official repositories are not built for Haskell developers, `haskell-` packages distributed there are only for programs written in Haskell. To save disk space, Haskell executables do not bundle their dependencies, so libraries are stripped into independent packages, consistent with Haskell package management. Other languages' distributions follow the similar strategy, whereas a vexing problem arise particularly in Haskell packaging. Haskell packages are packaged and linked dynamically, but GHC does not provide a stable [ABI](https://en.wikipedia.org/wiki/Application_binary_interface), since its specific hash method acting on circular dependences, which lie ubiquitously involving tests, will cause the [soname](https://en.wikipedia.org/wiki/Soname) of shared libraries interdependent. Consequently, you may notice that the entire Haskell packages in [[community]](https://www.archlinux.org/packages/) are not [reproducible](https://reproducible.archlinux.org/). If a Haskell library changes, all dependent packages are required to be rebuilt. For us, it is inevitable to rebuild and reinstall all tools which depend on those shared libraries after updating `haskell-` packages. Some users choose to avoid getting involved this sort of cheating, using static version Haskell programs as alternative. However, as a Haskell developer, we can make full use of these shared libraries.

## Configure Cabal

We will use [Cabal](https://cabal.readthedocs.io/en/3.2/) without sandboxes as our build tool, and system level GHC as our compiler. Let's install them via system package management tool:

```
# pacman -S ghc cabal-install
```

Generate the configuration and update hackage index as normal:

```
$ cabal update
```

Cabal are able to find system Haskell packages installed by pacman. If you try to use Cabal directly to compiling your project now, you will get:

> Could not find module ‘Prelude’...

indicating that many packages which would come with GHC are missing. This is because currently system Haskell packages provide only dynamic linked shared libraries, which can be used only when GHC is running in dynamic. So we have to configure `~/.cabal/config` as following:

```
library-vanilla: False
shared: True
executable-dynamic: True
program-default-options
  ghc-options: -dynamic
```

And if we want to install a Haskell program from cabal, we have to run:

```
cabal install --ghc-options=-dynamic [package to install]
```

to let Cabal call GHC enabled dynamic linking. 

## Install the language server

Personally, I would recommend [haskell-language-server](https://github.com/haskell/haskell-language-server), which is active in developing and provide unprecedented coding experience. Because we choose using dynamic GHC, we have to compile HLS by ourselves with dynamic option. Clone the source code:

```
$ git clone https://github.com/haskell/haskell-language-server --recurse-submodules
$ cd haskell-language-server
```

**IMPORTANT:** Configure the HLS project locally:

```
$ cabal configure --disable-library-vanilla --enable-shared --enable-executable-dynamic --ghc-options=-dynamic
```

Finally install it:

```
$ ./cabal-hls-install latest
```

Next step is choosing your favorite editor, and installing following the instruction in HLS. That's it, happy coding!

## Conclusion

Indeed, we have encountered the first impediment in installing HLS. Using dynamic GHC with system Haskell packages is double-edged, suggesting that we have to face various lurking issues.

Pros:

* it's impossible to get stuck into dependency hell (we always use the latest Haskell packages)
* far less disk usage is required

Cons:

* out-of-date packages are not available (so there will be slightly fewer libraries we can use)
* programs involving GHC should be given special treatment (make sure GHC is called with dynamic flag)

The last is kind of troublesome, because maintainers should patch the source of those programs to let them call GHC properly. Here are some examples:

* [Agda](https://github.com/archlinux/svntogit-community/blob/59c345b179aee0d71aca0df5974056bb0ac15ae2/trunk/PKGBUILD#L31)
* [xmonad](https://github.com/archlinux/svntogit-community/blob/f06b6574b90addc54a67c664532b8175496e2495/trunk/dynamic-compilation.patch#L8)
* [doctest](https://github.com/archlinux/svntogit-community/blob/833e37fbe6de1f07e106d81e9e0ef9e08f0513ad/trunk/PKGBUILD#L20)
* ...

Overall, it seems that we'd better don't touch these dynamic things in Haskell developing... Anyway, I'm posting this to illustrate that working with `haskell-` packages is possible. It is worth mentioning that **all Haskell packages**, and even **three tenths of entire Arch Linux packages** are maintaining by [felixonmars](https://github.com/felixonmars) individually, who is dedicated to these unpaid contributions. As an Arch Linux user, I would like to express my high respects to his greatest professionalism and responsibility. Next time, I will introduce my Haskell packaging tool [arch-hs](https://github.com/berberman/arch-hs), which can be used by both Arch Linux Haskell packagers and Haskell developers.

