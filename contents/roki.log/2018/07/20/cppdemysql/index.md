---
title: Connector/C++ で MySQL を操作
date: 2018-07-20 00:31:00
tags: C++
---


学校の関係で MySQL を触る機会が増えてきたので, MySQL の C++ 向けライブラリを一度触っておこうという忘備録.

<div class="p-3">
<ul class="nav nav-tabs">
  <li class="nav-item">
    <a href="#tab1" class="nav-link active" data-toggle="tab">sql_executor.hpp</a>
  </li>
  <li class="nav-item">
    <a href="#tab2" class="nav-link" data-toggle="tab">main.cpp</a>
  </li>
  <li class="nav-item">
    <a href="#tab3" class="nav-link" data-toggle="tab">Makefile</a>
  </li>
  <li class="nav-item">
    <a href="#tab4" class="nav-link" data-toggle="tab">docker-compose.yml</a>
  </li>
</ul>
</div>

<div class="tab-content" style="max-height: 510px; overflow: scroll;">
  <div id="tab1" class="tab-pane active">
    <script src="https://gist.github.com/falgon/2ade37d9ceb914f2c0dbd16c6271d98a.js?file=sql_executor.hpp" type="text/javascript"></script>
  </div>
  <div id="tab2" class="tab-pane">
    <script src="https://gist.github.com/falgon/2ade37d9ceb914f2c0dbd16c6271d98a.js?file=main.cpp" type="text/javascript"></script>
  </div>
  <div id="tab3" class="tab-pane">
    <script src="https://gist.github.com/falgon/2ade37d9ceb914f2c0dbd16c6271d98a.js?file=Makefile" type="text/javascript"></script>
  </div>
  <div id="tab4" class="tab-pane">
    <script src="https://gist.github.com/falgon/2ade37d9ceb914f2c0dbd16c6271d98a.js?file=docker-compose.yml" type="text/javascript"></script>
  </div>
</div>

これを実行すると, 次のように, 実在しそうでしなさそうな, 妙に怪しい雰囲気[^1]の一覧が出力される.

```
|顧客番号| C001 |氏名| 青山 花子        |郵便番号| 958-3626     |住所| 大分県横浜市瀬谷区台場12丁目23番18号 勝どきコーポ435     |電話番号| 090-3537-6380
|顧客番号| C002 |氏名| 廣川 翔太        |郵便番号| 297-3630     |住所| 群馬県西多摩郡奥多摩町平須賀14丁目5番2号 コーポ台東850   |電話番号| 88-1940-6921
|顧客番号| C003 |氏名| 田辺 浩  |郵便番号| 596-4390     |住所| 大分県いすみ市虎ノ門虎ノ門ヒルズ森タワー31丁目11番1号    |電話番号| 080-4923-6200
|顧客番号| C004 |氏名| 井上 知実        |郵便番号| 903-5859     |住所| 岩手県北区箪笥町7丁目6番2号 高田馬場クレスト753  |電話番号| 090-1267-5646
|顧客番号| C005 |氏名| 浜田 明美        |郵便番号| 644-0375     |住所| 北海道武蔵野市蟇沼41丁目6番4号 パレス南赤田445   |電話番号| 090-3897-3724
```

見てのとおり, docker-compose を使って MySQL コンテナを立てて, そこに要求を投げる.
内容は, データベースを新たに作成してテーブル, 値を追加して, 追加項目を全て`SELECT`するだけ[^2].
C++ には RAII があるので, コード中でもそうなのだが`SET AUTOCOMMIT = 0`,`START TRANSACTION`のあとに,
`COMMIT`をし忘れるなんてことを防げるのが良い.
一応 SQL 文が格納されている Range かイテレータを渡せば順次実行, 単一の SQL を渡せばもちろんそれを実行するようにしている.
実際に必要に迫られているわけではないのでなんとも言えないが, 現時点では, 
それなりに気持ちよく書ける程度にラッピングできたような気がする[^3].
1 点この Connector/C++ に対する不満を申し上げるとすれば, C++11 までにしか未だ対応していない点だろう.

しかし久しぶりに C++ 書いた. なんだろうこの実家感は.

[^1]: この個人情報と思わしきリストは, コード内のコメントにも書いたが, 偽の個人情報をランダムに生成する Python 製の[ライブラリ](https://github.com/joke2k/faker)で自動生成したものである. 同ライブラリは日本語だけでなく, 様々な国の様式に沿ったそれらしき文字列を提供してくれるので, `hoge`や`foo`等に飽きたら, これを使えば良いのではないのだろうか :p
[^2]: なお, このインクルードしている Srook というものは, 私の自作ライブラリです. このブログに移行してくる前のブログでは, よく取り扱っていました.
[^3]: `playing::cppconn`とかいうふざけた名前空間だが.
