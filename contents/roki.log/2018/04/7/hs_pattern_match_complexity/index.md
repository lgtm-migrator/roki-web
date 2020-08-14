---
title: ghc パターンマッチの時間計算量
date: 2018-04-07 16:50:00
tags: Haskell
header-warn: この記事は, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>から移植された記事です. よって, その内容として, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>に依存した文脈が含まれている可能性があります. 予めご了承下さい.
---

[reddit](https://www.reddit.com/r/Haskell/comments/8aaqr2/how_does_Haskell_work_with_multiequation_functions/?ref=share&ref_source=link) で見かけて, ふと気になったのでメモ.
GCC で C/C++ コードの `switch` 文および `case` 節をコンパイルするとき, 
`case` 節の数が一定以上を超えると, ジャンプテーブルを利用したアセンブリが吐き出される[^1].
同様にして, ghc はパターンマッチで[ジャンプテーブルが用いられる場合がある](https://github.com/ghc/ghc/blob/7ff6023537fdef32bbe9b4c357012d705d9b931f/compiler/cmm/CmmSwitch.hs). <!--more-->
以下, メーリングリスト[^2]から, パターンマッチの時間計算量に関する言及について一部引用.

<blockquote>
(snip) 
<strong>To answer you question, O(1) for simple patterns, but it really depends on
the complexity of the pattern-matching expression and the Core-to-Core
transformations that GHC applies.</strong> To truly understand the complexity, you
need take a look at the Core/STG dump (I prefer STG since it's simple). 
If you have any particular code samples you'd like me to help you analyze, I'd be happy to do so.<br>
A basic example:
<pre>
data Color = Red | Blue | Green
isRed Red = True
isRed _ = False
</pre>
GHC will transform this to
<pre>
isRed x = case x of { Red -True; DEFAULT -False }
</pre>
You can think of a case as a switch expression in your standard imperative
languages. A case expression will evaluate the thunk 'x' and perform a
switch on the tag of the result. Each data constructor has an integer tag
associated with it which will be the target of the switch. <strong>So the time
complexity of `isRed` will be the time complexity of thunk evaluation which
is impossible to predict because a thunk can be incredibly complex. Lazy
evaluation is not so easy to analyze. It is highly context-sensitive.</strong>(snip)<br>
The way you're measuring algorithmic complexity does carry over to <strong>the lazy
setting provided it's single-threaded because the order of execution is
purely determined by the STG Code. In the concurrent lazy setting, it's a
bit trickier since lightweight locking mechanisms occur when multiple
threads evaluate the same thunk, making it non-deterministic.</strong>
</blockquote>

## 参考文献

* [GHCのこと](http://www.kotha.net/hperf/ghc.html)
* [A Term Pattern-Match Compiler Inspired by Finite Automata Theory](https://pdfs.semanticscholar.org/c0d6/f0225c5140d1528f35d187f070d415f33ed6.pdf)
* [OLD DESIGN DOCUMENT: The semi-tagging optimisation](https://ghc.Haskell.org/trac/ghc/wiki/SemiTagging)
* [Implementing Lazy Functional Languages on Stock Hardware: The Spineless Tagless G-machine](https://www.microsoft.com/en-us/research/publication/implementing-lazy-functional-languages-on-stock-hardware-the-spineless-tagless-g-machine/?from=http%3A%2F%2Fresearch.microsoft.com%2Fapps%2Fpubs%2Fdefault.aspx%3Fid%3D67083)
* [Compiling pattern matching (有料)](https://link.springer.com/chapter/10.1007%2F3-540-15975-4_48)

[^1]: [godbolt](https://godbolt.org/g/7N34EF), [gcc-mirror](https://github.com/gcc-mirror/gcc/blob/47f1fd04f7e813fbfe041d7bde9edeadbef35f9d/gcc/params.def#L1099-L1107), [gcc-mirror](https://github.com/gcc-mirror/gcc/blob/c4b26cae233b9462ce32aa14464e916c43332c2d/gcc/tree-switch-conversion.c#L1701-L1710), 
[^2]: [time complexity of pattern matching - mail.Haskell](https://mail.Haskell.org/pipermail/beginners/2016-July/017010.html), [time complexity of pattern matching - mail.Haskell](https://mail.Haskell.org/pipermail/beginners/2016-July/017012.html)
