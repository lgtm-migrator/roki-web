---
title: 個人ページ, ブログを移行した
date: 2020-08-09 06:38:00
tags: Haskell
---

[旧個人ページ](https://falgon.github.io/roki/ )及び,
[旧ブログ](https://falgon.github.io/roki.log/)を本ウェブサイト 
([www.roki.dev](/)), 
本ブログ ([www.roki.dev/roki.log](/roki.log), [www.roki.dev/roki.diary](/roki.diary)) に移行した.
以下では, 移行した経緯や技術的概要, 本サイトおよびブログの方針について紹介したい.

<!--more-->

## 経緯

2016 年に[はてなブログ (Roki のチラ裏)](https://roki.hateblo.jp/)でブログを書き始めた[^1]後,
2018 年頃に Gihub pages にブログを移行, 個人ページを別途開設,
今回で新たにブログ及び個人ページを統合した形で移行ということで, これが三度目の移行となる[^2].

### 旧ブログ

移行前の旧ブログの構成では, static site generator
である [pelican](https://github.com/getpelican/pelican) を使っていた. 
pelican は python 製の static site generator で, 
[テンプレート](http://www.pelicanthemes.com/)や[プラグイン](https://github.com/getpelican/pelican-plugins)が充実しており,
設定も非常に少ない記述から簡潔に行えるようになっていて, 
非常に使い勝手の良いツールであった.
自分の場合は, [nikhil-theme](https://github.com/gunchu/nikhil-theme)
を元に[拡張して](https://github.com/falgon/nikhil-theme)利用しており,
いくつかの機能の追加実装や bug fix, 依存関係の更新作業などを行っていた.

記事の執筆は Markdown, D3.js や emscripten を導入していたので, 
記事内においては簡単なシミュレータや計算の視覚化などでヌルヌル動かしたり,
遊んだりできるようにしていた.

執筆時には \\(\LaTeX\\) が意図通り描画できているか等で 
瞬時にプレビューを見たかったため, 
記事内の更新に合わせて記事のリビルド, 
ブラウザの自動リロードがされる[小さなツール](https://gist.github.com/falgon/5d3fe6838e7f6cb4090823df417680e5)を作り, 実用していたのでブログ記事執筆時のストレスもそこまではなかった.

これまでを振り返ると, 私個人としてもそこまでの不満はなかったのではないかというような気がしてくるが,
全く不満がなかったかというとそうではなかった.

* 元テンプレートの Bootstrap のバージョンが低く, この更新作業コスト対利益を考えるとやる気が出ない
* うんこ

### 旧個人ページ

移行前の旧個人ページでは, 
当時のフロントエンド技術として非常に流行っていた Typescript + React を使って構築していた.


## ブログを二つに分ける理由


[^1]: それよりも以前はアメーバブログで技術系の記事を書いていたが, 随分前にもう消してしまっていた...
[^2]: 特別意識しているわけではないが, こう見ると二年周期で移行している気がする... (もう移行はしないつもり)
