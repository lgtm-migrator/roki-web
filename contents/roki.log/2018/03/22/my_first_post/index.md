---
title: ハローワールド
date: 2018-03-22 16:50:00
tags: 
header-warn: この記事は, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>から移植された記事です. よって, その内容として, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>に依存した文脈が含まれている可能性があります. 予めご了承下さい.
---

<article class="message is-danger">
<div class="message-body">
<i class="fas fa-info-circle"></i>
この記事は, 旧ブログに依存した文脈を含んでいます.
すなわち, [旧ブログ](https://falgon.github.io/roki.log)への移転時に書かれた記事であり, 
本ブログへの移転に関する記事ではありません. これは記録として残されています.
</div>
</article>

## ブログを移転した

これまでは約二年ほど [はてブロ](https://roki.hateblo.jp/)に, メモや学習ログなどを投稿し, 利用させて頂いていたが,
$\LaTeX$ の記述においてはてブロ固有の独特な記法を必要とされることがあり, これらの点で
少し不便に感じていたので, 新たにブログを立ち上げこちらに移転することとした. 
このブログは github pages でホスティングされており, 
[Pelican](https://github.com/getpelican) という Static site generator によって成り立っている. 
またテーマは, [nikhil-theme](https://github.com/gunchu/nikhil-theme) 
を[フォークして](https://github.com/falgon/nikhil-theme)利用させて頂いている.

<!--more-->

## 移転で行った作業

流れとしては, 通常通り, `pip` で Pelican を導入し, `pelican-quickstart` によって必要なディレクトリ階層と, 
最低限のファイル構成を得て, そこから諸々の設定を行った. 
Pelican + github pages でブログを管理する事例はとても多く, 
また[ドキュメント](http://docs.getpelican.com/)も充実しており, 特別困ることはなかったが, 
いくつかテーマの修正, 機能追加などを行った. 
**本ブログ構造や管理に関する変更の記録は, 本エントリにて随時更新している**.

* HTTP コンテンツが入り混じっていたため, これを[修正した](https://github.com/falgon/nikhil-theme/commit/c9e2b23e1a46ac35f66f5178cb16e1d5ea3f8a16).
* [Pelican の 3.7.0 から `PAGES` という context variable が `pages` に変更されており](http://docs.getpelican.com/en/stable/changelog.html#id2), そのままでは正常にレンダリングされないためこれを[修正した](https://github.com/falgon/nikhil-theme/commit/5bdb0be4eea2636963caed081a8909822eaab297).
* favicon の生成を ImageMagick で行い, これを [Wiki](https://github.com/getpelican/pelican/wiki/Tips-n-Tricks#second-solution-using-static_paths) 通りに設定した. 
* コメント機能となる DISQUS を追加した.
* CC ライセンスを追加した.
* 検索機能を追加した.
* 404 ページを追加した.
* [pelican_dynamic プラグインを fork](https://github.com/falgon/pelican_dynamic) し, 少し修正して導入して d3.js が動くようにした.
* テーマ内臓のシンタックスハイライトのスタイルシートが Mathjax の利用するスタイルシートと衝突しており([該当部分](https://github.com/SimonEnsemble/SimonEnsemble.github.io/blob/d41a60d001fb2c18cb123894bd9afbe76fadada1/_sass/_syntax.scss#L44-L45)), 数式が緑色でレンダリングされてしまっていた. これを, ワークアラウンドとして [Mathjax の使うクラスに対して`color: inherit;`を指定](https://github.com/falgon/nikhil-theme/commit/466dce1d0e17b8b48c506cef7a7e75321e61c162)し, 修正した[^1].
* [python-livereload](https://livereload.readthedocs.io/en/latest/) を用いて記事のライブビューができるようにした. バックグラウンドで立ち上がって欲しいので, それら諸々をシェルスクリプトで書いた (以下の gist をサブモジュールとしてマスターブランチに登録している).

<div class="tabs is-toggle is-boxed is-centered mb-0" id="tabs">
<ul>
<li class="is-active" data-tab="1">
<a>
<span class="icon is-small"><i class="fab fa-python fa-fw"></i></span>
<span>live_preview.py</span>
</a>
</li>
<li data-tab="2">
<a>
<span class="icon is-small"><i class="fas fa-file-code fa-fw"></i></span>
<span>live_preview.sh</span>
</a>
</li>
</ul>
</div>

<div id="tab-content" style="max-height: 400px; overflow-y: scroll;">
<div class="is-active acontent" data-content="1">
<script 
    src="https://gist.github.com/falgon/5d3fe6838e7f6cb4090823df417680e5.js?file=live_preview.py" 
    type="text/javascript">
</script>
</div>
<div class="acontent" data-content="2">
<script 
    src="https://gist.github.com/falgon/5d3fe6838e7f6cb4090823df417680e5.js?file=live_preview.sh" 
    type="text/javascript">
</script>
</div>
</div>
<script type="text/javascript" src="/js/uniq_tab.js"></script>

* ローカルマシンでのサイト生成をやめ, 特定ブランチへのプッシュをトリガーに Bitbucket Pipeline によってサイト生成, デプロイを行うこととした.

## 構造

フォークしたテーマと [pelican-plugins](https://github.com/getpelican/pelican-plugins) をマスターブランチのサブモジュールとして置いた.
マスターブランチには, ブログ記事の下書きなども貯めようと思っていたため, 特別これを公開する意味はない.
そこで, マスターブランチは private リポジトリとして bitbucket に, gh-pages ブランチは github にホスティングして頂くこととした[^2].

* [master](https://bitbucket.org/r0ki/roki.log) (非公開なのでここに貼っても特別意味はないが)
* [gh-pages](https://github.com/falgon/roki.log) 

gh-pages への反映は, ghp-import を利用している. 
記事を公開しようとするたびに毎度ブランチをチェックアウトするのは面倒なので,
Makefile にコマンドを書いておき,
そのコマンド一発で記事の生成と gh-pages へのプッシュを行えるようにした. 
また, bitbucket には標準搭載の CI (bitbucket Pipelines) があるので,
master ブランチへのプッシュをトリガーに, 自動で記事の生成テストを行うようにしている.

[^1]: おそらく [#349](https://github.com/getpelican/pelican-plugins/issues/349) の問題も, この関係なのではないかと思われる.
[^2]: 2019/1/7, [The GitHub Blog](https://github.blog/2019-01-07-new-year-new-github/) でアナウンスされた通り, マイクロソフトに買収された GitHub はプライベートリポジトリの機能を一般ユーザへ無償で公開した. 従って, プライベートリポジトリの機能を使うために, GitHub 以外のサービスを用いるということの必然性はなくなったわけであるが, なんとなくベンダーロックインしてしまうことに抵抗があるので, この構成で運用を続けていく予定.
