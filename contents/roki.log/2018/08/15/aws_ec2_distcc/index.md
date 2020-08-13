---
title: AWS EC2 の各種環境を自動構築して distcc による分散コンパイルを実行する
date: 2018-08-15 00:00:00
tags: AWS, golang, Networking
header-warn: この記事は, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>から移植された記事です. よって, その内容として, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>に依存した文脈が含まれている可能性があります. 予めご了承下さい.
---

クラウド上でなにか作れというような大学の課題で, 
入力パラメータに応じて AWS EC2 インスタンス及びネットワーク周辺と distcc の環境構築を実行して,
その上で分散コンパイルをして S3 へアップロードできれば, 
そこそこクラウドでやった意味があるといえるのかななどと思いつき, 軽い気持ちで作ってみた記録.

### 構成

構成そのものはかなり単純だと思う. はじめに, いくつかのパラメータを指定する. 
数は多いが, AWS EC2 の環境構築に最低限必要となるような要素に限られているはず.

<script src="https://gist-it.appspot.com/https://github.com/falgon/edcc/blob/5f4cd53830691275e5c250ef2051adca9d1162d6/README.md?slice=10:44"></script>

ここで指定したパラメータに応じて, 環境を構築する. 
その際, AWS のユーザーデータ[^1]機能を使って, distcc とコンパイラ[^2]のセットアップ, 
ホストインスタンス(実際にコンパイルを実行するインスタンス)の決定,
各インスタンスの環境構築における進捗の同期等を行い,
ビルドスクリプトを実行する.

一応, ここに酷いアクティビティ図がある.

<div style="height: 400px; overflow-y: scroll;">
<img width="700" src="https://raw.githubusercontent.com/falgon/edcc/4a052baece667b6baf6d3e41ff3f0741faee5bed/assets/fig.png" alt="ugly activity diagram">
</div>

### 実装

実装は, 次のリポジトリで管理している.

<p style="text-align: center;">
<i class="fab fa-github" style="font-size: large; margin-right: 5px;"></i>
<a href="https://github.com/falgon/edcc">falgon/edcc - Simple and tiny comprehensive management tool for distributed compilation using distcc on AWS EC2.</a>
</p>

まず, 起動したインスタンスのすべてに必要となるパッケージのインストールやセットアップを実行する必要があるが,
これは構成にて述べたように, AWS EC2 の機能のうちの 1 つである, ユーザーデータ[^1]を利用して実行することとした.

予め使用言語に対する指定として, Go で実装することを定められていたので, 
今回の実装を Go で行ったことに対する深い意味合いは特別ないが,
とりあえずそこまで深く考えず Go の便利な標準パッケージ, `text/template`を利用して,
一程度の情報伝達を行うこととした. 
`text/template`は, かなりお手軽に[設定ファイルの生成](https://falgon.github.io/roki.log/posts/2018/%207月/23/ec2failover/)のほか,
トークンの衝突さえなければ, コードに直接埋め込むことができるので, それを元にコードの生成をすることもでき, 大変良い.

ビルドスクリプトおよびセットアップスクリプトは, それぞれ, 
ビルド実行のスクリプト[^3]と,
ユーザーデータとして渡される, 
必要パッケージのセットアップスクリプト[^4]のことを示している.
ビルドスクリプトは, 以下に示す変数を利用して任意に記述してもらう[^5]ものとして,
セットアップスクリプトは, 殆どの場合, 中身を弄る必要はないと思われる.
今のところ, 各スクリプトは bash スクリプトとして記述する必要がある.
それぞれで利用できる変数は, 次の通りである.

<script src="https://gist-it.appspot.com/https://github.com/falgon/edcc/blob/5f4cd53830691275e5c250ef2051adca9d1162d6/README.md?slice=46:63"></script>

大体各変数の予測はつくだろうし, 説明にもあるとおりなのだが, 一点, 必ずビルドスクリプト内に記述しなければならないのは,
`{{.build_success}}`または`{{.build_failed}}`である.
これは, `{{.Include_WriteStatus}}`を予め記述しておくことで利用できるようになる.
それぞれビルドの成功, また失敗といった結果を通知するための命令(内部はただの bash 関数)であるが,
このどちらをも指定しなかった場合, ビルドはまだ終わっていないと認識して, 
永遠に停止することはない(マネジメントコンソールや awscli などで自分でクリーンナップを行う必要がある.).

この仕様は, Travis CI を利用しているときに, 
ステータスコードが 0 以外の場合においても, 処理を続行したいときが個人的には何度かあったことに由来している.

また, セットアップの進捗をインスタンス間でどのように同期するかであるが, 
EC2 においても各メタ情報を HTTP リクエストで取得できることを真似て,
各インスタンスで nginx による HTTP サーバ[^6]を稼働させ, そのトップページに自身の状態の JSON を出力しておき,
それを curl で得るということにした.

結局, そのほかの詳細はリポジトリ内の README を見てほしい.
実行例は次の通りである.

<script src="https://gist-it.appspot.com/https://github.com/falgon/edcc/blob/5f4cd53830691275e5c250ef2051adca9d1162d6/README.md?slice=68:102"></script>

### 感想

とにかく時間のない中であったので, 
妥協してしまった点がいくらかあり,
その点で個人的には悔しい部分があるが, 
今回の実装で大分総合的に EC2 関係の IaaS やら SaaS 周りを活用できたかと思うので, 
取り組めたこと自体には満足をしている.

別の話題だが,
講義内で利用する AWS リソースの支払いは, ありがたい事に大学側が持っていてくれていたのだが,
これでもう講義が終わってしまうので, 犬が西向きゃ尾は東というものであるが,
続けて自分で本格的に取り組むのには, ある程度の出費が必要となる.
今のところ, まだ無料利用枠は残っているので, それを利用できることが救いであるが, 
講義でよく使っていた t2.medium と無料利用枠対象の t2.micro とでは, 
やはり処理性能に若干の違いを感じる.
なんだか今回のエントリには, やたらと諺が出てきて自分も奇怪に感じるが, 
まあこれは, 起きて半畳寝て一畳, 天下取っても二合半ということなのだろう.

[^1]: "Running Commands on Your Linux Instance at Launch" <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html> 2018 年 8 月 15 日アクセス.
[^2]: デフォルト([setup.sh](https://github.com/falgon/edcc/blob/5f4cd53830691275e5c250ef2051adca9d1162d6/src/setup.sh))では GCC 8.1.0
[^3]: 例として, 厳密性を多いに省けば, Travis CI で実行される, `.travis.yml`の`script`セクションようなもの.
[^4]: 同じく, Travis CI で実行される, `.travis.yml`の`install`セクションのようなもの.
[^5]: [build_script_example/](https://github.com/falgon/edcc/tree/5f4cd53830691275e5c250ef2051adca9d1162d6/build_script_example)にいくつかのサンプルがある.
[^6]: このとき 80 番ポートを利用するが, デフォルトの設定では VPC を 10.0.0.0/16, サブネットを 10.0.0.0/24 とし, 80 番ポートのインバウンド設定を, セキュリティグループにより 10.0.0.0/24 と設定するので, 外部からの HTTP アクセスに対して応答することはない. よって, インスタンスの状態がインターネットに漏れてしまうといった懸念は必要ない. なお自動構築時, このサブネット中にすべてのインスタンスを設定するため, 自ずと分散コンパイルを行うインスタンスの最大数は 256 となる. それよりも増やしたいのであれば, 単にパラメータを変えればよいが, あまり台数を増やしても, 然程効果はないと思われる.
