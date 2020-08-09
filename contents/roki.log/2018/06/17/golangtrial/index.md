---
title: golang 始めたてメモ
date: 2018-06-17 10:45:00
tags: golang
---

大学の授業で AWS の API を触る機会があり, その際に golang を使うようなので(API 一覧には, C++ 版もあるようなので, 個人的には C++ が良かった), 特に意義のないコードを取り敢えずいくつか書いてみている.
ある程度文法がわかったら, まずはとにかくクイックソートを書くよ.

<script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=qsort.go" type="text/javascript"></script>

初めは`append`とスライスを乱用すれば, 関数型言語っぽく再帰が書けてスッキリするので, `reverse`を
```go
func reverse(ar []int) []int {
    var rev func([]int, []int) []int
    rev = func(a, b []int) []int {
        if len(a) == 0 {
            return b
        }
        return rev(a[1:], append([]int{a[0]}, b...))
    }
    return rev(ar, make([]int, 0))
}
```
というように書いてみていたのだが, golang は現在のところ, TCO が効かない[^1]ようなのでやめた. 

そして一通り A Tour of Go の Exercise をやってみた.

<div class="p-3">
<ul class="nav nav-tabs">
  <li class="nav-item active">
    <a href="#tab1" class="nav-link active" data-toggle="tab">LoopsandFunctions</a>
  </li>
  <li class="nav-item">
    <a href="#tab2" class="nav-link" data-toggle="tab">Slices</a>
  </li>
  <li class="nav-item">
    <a href="#tab3" class="nav-link" data-toggle="tab">Maps</a>
  </li>
  <li class="nav-item">
    <a href="#tab4" class="nav-link" data-toggle="tab">FibonacciClosure</a>
  </li>
  <li class="nav-item">
    <a href="#tab5" class="nav-link" data-toggle="tab">Stringers</a>
  </li>
  <li class="nav-item">
    <a href="#tab6" class="nav-link" data-toggle="tab">Erros</a>
  </li>
  <li class="nav-item">
    <a href="#tab7" class="nav-link" data-toggle="tab">Readors</a>
  </li>
  <li class="nav-item">
    <a href="#tab8" class="nav-link" data-toggle="tab">rot13Reader</a>
  </li>
  <li class="nav-item">
    <a href="#tab9" class="nav-link" data-toggle="tab">Images</a>
  </li>
  <li class="nav-item">
    <a href="#tab10" class="nav-link" data-toggle="tab">Equivalent Binary Trees</a>
  </li>
  <li class="nav-item">
    <a href="#tab11" class="nav-link" data-toggle="tab">Web Crawler</a>
  </li>
</ul>
</div>

<div class="tab-content">
  <div id="tab1" class="tab-pane active">
    <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=LoopsandFunctions.go" type="text/javascript"></script>
  </div>
  <div id="tab2" class="tab-pane">
    <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=Slices.go" type="text/javascript"></script>
  </div>
  <div id="tab3" class="tab-pane">
    <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=Maps.go" type="text/javascript"></script>
  </div>
  <div id="tab4" class="tab-pane">
    <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=FibonacciClosure.go" type="text/javascript"></script>
  </div>
  <div id="tab5" class="tab-pane">
    <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=Stringers.go" type="text/javascript"></script>
  </div>
  <div id="tab6" class="tab-pane">
    <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=Erros.go" type="text/javascript"></script>
  </div>
  <div id="tab7" class="tab-pane">
    <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=Readers.go" type="text/javascript"></script>
  </div>
  <div id="tab8" class="tab-pane">
    <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=rot13Reader.go" type="text/javascript"></script>
  </div>
  <div id="tab9" class="tab-pane">
    <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=Images.go" type="text/javascript"></script>
  </div>
  <div id="tab10" class="tab-pane">
    <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=EquivalentBinaryTrees.go" type="text/javascript"></script>
  </div>
  <div id="tab11" class="tab-pane">
    <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=WebCrawler.go" type="text/javascript"></script>
  </div>
</div>

最後の Web Crawler の問題には他と比べて時間がかかってしまったが, 比較的楽しみながらできたので良かったのではないだろうか.


[^1]: 参考: "proposal: Go 2: add become statement to support tail calls #22624" <https://github.com/golang/go/issues/22624> 2018 年 6 月 17 日アクセス. "The Go Programming Language - Release History" <https://golang.org/doc/devel/release.html> 2018 年 6 月 17 日アクセス.
