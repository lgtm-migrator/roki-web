---
title: C++ で Data.Tuple.Extra っぽいもの
date: 2018-06-14 16:50:00
tags: C++
header-warn: この記事は, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>から移植された記事です. よって, その内容として, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>に依存した文脈が含まれている可能性があります. 予めご了承下さい.
---

ふと, C++ でもこんなような記述, 普通に出来るべきなんじゃないかなぁと思った.
```Haskell
Prelude> :m +Data.Tuple.Extra
Prelude Data.Tuple.Extra> uncurry (+) $ first (*2) $ dupe 42
126
Prelude Data.Tuple.Extra> uncurry (+) $ (+42) &&& (*2) $ 42
168
```
取り敢えず, 似たような構文で同じような処理となるように[作ってみた](https://github.com/falgon/SrookCppLibraries/tree/develop/srook/tuple/utility).
C++11 以降では, Variadic templates が使えるので, [Data.Tuple.Extra](http://hackage.Haskell.org/package/extra-1.6.8) ([Control.Arrow](http://hackage.Haskell.org/package/base-4.11.1.0/docs/Control-Arrow.html)) の`first`, `second`関数のように, タプルの 1 番目, 2 番目に作用させるといった関数をそれぞれ別個作る必要はない.
従って, これを作用させるタプルのインデックス値で指定できるようにしてみた(`srook::tuple::utility::nth`). 他のものも同様, なんとなく似た感じになるように, なんとなくやってみた(?).

<script src="https://gist-it.appspot.com/github.com/falgon/SrookCppLibraries/blob/develop/tests/tuple/utility/test.cpp"></script>

結局, 関数を次々と呼び出す構文をすっきりと見せるために, Range TS のように`operator|`をオーバーロードした.
C++14 以降では Return type deduction が使えるが, やはり型を明示的に書きたかったので, そのあたりでいくらかテンプレートメタプログラミングをした.
個人的には久しぶりに C++ を書く上で, これが一番 C++ らしさを感じたし, 最も楽しい部分であった. <span style="font-size: x-small">このブログに移行してきて, 初の日記っぽい内容の投稿となった気がする.</span>
