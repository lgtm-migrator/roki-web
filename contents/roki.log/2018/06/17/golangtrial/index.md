---
title: golang 始めたてメモ
date: 2018-06-17 10:45:00
tags: golang
header-warn: この記事は, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>から移植された記事です. よって, その内容として, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>に依存した文脈が含まれている可能性があります. 予めご了承下さい.
---

大学の授業で AWS の API を触る機会があり, 
その際に golang を使うようなので
(API 一覧には, C++ 版もあるようなので, 個人的には C++ が良かった), 
特に意義のないコードを取り敢えずいくつか書いてみている.
ある程度文法がわかったら, まずはともかくクイックソートを書いてみたわけだが...

<!--more-->

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

<div class="tabs is-toggle is-boxed is-centered mb-0" id="tabs">
<ul>
<li class="is-active" data-tab="1">
<a>
<span class="icon is-small"><i class="fas fa-file-code fa-fw"></i></span>
<span>LoopsandFunctions</span>
</a>
</li>
<li data-tab="2">
<a>
<span class="icon is-small"><i class="fas fa-file-code fa-fw"></i></span>
<span>Slices</span>
</a>
</li>
<li data-tab="3">
<a>
<span class="icon is-small"><i class="fas fa-file-code fa-fw"></i></span>
<span>Maps</span>
</a>
</li>
<li data-tab="4">
<a>
<span class="icon is-small"><i class="fas fa-file-code fa-fw"></i></span>
<span>FibonacciClosure</span>
</a>
</li>
<li data-tab="5">
<a>
<span class="icon is-small"><i class="fas fa-file-code fa-fw"></i></span>
<span>Stringers</span>
</a>
</li>
<li data-tab="6">
<a>
<span class="icon is-small"><i class="fas fa-file-code fa-fw"></i></span>
<span>Erros</span>
</a>
</li>
<li data-tab="7">
<a>
<span class="icon is-small"><i class="fas fa-file-code fa-fw"></i></span>
<span>Readors</span>
</a>
</li>
<li data-tab="8">
<a>
<span class="icon is-small"><i class="fas fa-file-code fa-fw"></i></span>
<span>rot13Reader</span>
</a>
</li>
<li data-tab="9">
<a>
<span class="icon is-small"><i class="fas fa-file-code fa-fw"></i></span>
<span>Images</span>
</a>
</li>
<li data-tab="10">
<a>
<span class="icon is-small"><i class="fas fa-file-code fa-fw"></i></span>
<span>Equivalent Binary Trees</span>
</a>
</li>
<li data-tab="11">
<a>
<span class="icon is-small"><i class="fas fa-file-code fa-fw"></i></span>
<span>Web Crawler</span>
</a>
</li>
</ul>
</div>

<div id="tab-content" style="max-height: 400px; overflow-y: scroll;">
  <div class="is-active acontent" data-content="1">
  <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=LoopsandFunctions.go" type="text/javascript"></script>
  </div>
  <div class="acontent" data-content="2">
  <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=Slices.go" type="text/javascript"></script>
  </div>
  <div class="acontent" data-content="3">
  <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=Maps.go" type="text/javascript"></script>
  </div>
  <div class="acontent" data-content="4">
  <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=FibonacciClosure.go" type="text/javascript"></script>
  </div>
  <div class="acontent" data-content="5">
  <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=Stringers.go" type="text/javascript"></script>
  </div>
  <div class="acontent" data-content="6">
  <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=Erros.go" type="text/javascript"></script>
  </div>
  <div class="acontent" data-content="7">
  <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=Readers.go" type="text/javascript"></script>
  </div>
  <div class="acontent" data-content="8">
  <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=rot13Reader.go" type="text/javascript"></script>
  </div>
  <div class="acontent" data-content="9">
  <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=Images.go" type="text/javascript"></script>
  </div>
  <div class="acontent" data-content="10">
  <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=EquivalentBinaryTrees.go" type="text/javascript"></script>
  </div>
  <div class="acontent" data-content="11">
  <script src="https://gist.github.com/falgon/fe1ebde33f0da774e3030bfa4e8eb941.js?file=WebCrawler.go" type="text/javascript"></script>
  </div>
</div>
<script type="text/javascript" src="/js/uniq_tab.js"></script>

最後の Web Crawler の問題には他と比べて時間がかかってしまったが, 
比較的楽しみながらできたので良かったのではないだろうか.


[^1]: 参考: "proposal: Go 2: add become statement to support tail calls #22624" <https://github.com/golang/go/issues/22624> 2018 年 6 月 17 日アクセス. "The Go Programming Language - Release History" <https://golang.org/doc/devel/release.html> 2018 年 6 月 17 日アクセス.
