---
title: Haskell でリンクレイヤーにおける ICMP パケットの構築, 送受信および解析による ping の実装
date: 2018-09-15 00:00:00
tags: Haskell, Networking
---

Haskell で低レイヤーのネットワークプログラミングをそういえばしたことがなかったので, 
何か実装してみたかったのだが特別ネタも思いつかないので,
とりあえずイーサヘッダ, IP ヘッダ等を含む, 生の ICMP Echo/Reply パケットを扱ってみることとした.

ICMP パケットは IP パケットであるので, 
通常は ICMP パケット部分のみを構築して`PF_INET`等で開いたソケットに送りつけたり,
`recv` 等すれば送受信においては必要十分であるが, 
これではあまり面白みがないので, リンクレイヤーから扱うこととした.

生の ICMP パケットを扱うということは, ICMP データの自作はもちろん, イーサヘッダ, IP ヘッダの自作が必要となる.
またイーサヘッダを自作するということは, MAC アドレスを解決しなければならないので,
最低限 ARP パケットの送受信および解析機能の自作が必要となることを意味する.
ARP パケットの自作を要するということは, デフォルトゲートウェイやサブネット環境などを取得する機能も必要である.
これらを自作してみた.

### 環境

環境は, 本エントリ末尾に記載のリポジトリ内にある 
[Vagrantfile](https://github.com/falgon/network-basal/blob/8ba27abae4fa69652756ef7941f6377d46b54eff/testenv/Vagrantfile) 
の通りで, ごく普通の Ubuntu 18.04 仮想マシンである.
テスト用途として, 同一プライベートネットワーク上にもう 1 つ同 OS のノードを用意している.

### ARP 

まずは冒頭で述べた理由より, ARP パケットの作成, 送受信および解析の機能を実装する必要がある.
ARP に関しては, RFC 826 を再確認しつつ実装した.
これは[以前 C++ で実装した](https://falgon.github.io/roki.log/posts/2018/%205月/01/detectPromiscuous/)ことがあったので,
とくに困ることはなかったが, Haskell では, とくにリンクレイヤにおいては, 
その肝心なパケットの送受信の手段があまり充実していないようで, それには少々戸惑った.
たとえば, 本エントリ執筆時点で, 同レイヤーのパケット送受信を
[Network.Socket](http://hackage.Haskell.org/package/network-2.8.0.0) モジュール等で実行することは不可能である.

当初は単に`PF_PACKET`, `SOCK_RAW`等で開いたソケットに書き込もうと思っていたので,
これは FFI で呼び出すしかないかと思ったが, ふと libpcap の Haskell ラッパーである
[pcap](http://hackage.Haskell.org/package/pcap) モジュールの存在を知り, これを利用させて頂くこととした.

今回簡単のため, ARP キャッシュを単に /proc/net/arp を読むことで済ませており, この点で手抜き仕様となっているので,
今後自前で ARP キャッシュを実装するか, /proc/net/arp との共和の良い方法を検討するかしたい.

この実装による副産物として, 
同一ネットワーク上の IP アドレスを指定すると, その MAC アドレスが得られる arp-exe という小さなアプリケーションができた.

```sh
$ sudo stack --allow-different-user exec arp-exe -- eth2 192.168.33.12 # リポジトリ内記載の vagrant 環境上で
Just 08:00:27:8b:b4:ae
```

### サブネットの判定とデフォルトゲートウェイの取得

目的対象ノードの MAC アドレスを取得する必要性は先に述べた通りで,
いまそれが同一ネットワーク上にあるならば, 単にそのノードを指定して ARP を送出すればよいのであるが,
そうでない場合, デフォルトゲートウェイに委託しなければならない[^1].
よって, まず実行ホストの NIC に対応するデフォルトゲートウェイをルーティングテーブル等から知る必要がある.
今回は Linux 上での動作を前提としているので, /proc/net/route を読めばよい.

次に, 目的対象ノードが到達範囲内にあるかどうかを判定するために, 
自身のサブネットを取得する必要がある. 
Linux 上でこれを行う方法としては, `getifaddrs`等を呼び出すことが考えられるが, 
既存のモジュール等でこれを自由に扱う手立てはどうにもないようであった.
これは仕方がないので, FFI を利用して`getifaddrs`を呼び出し, 取得することとした.

### その他

その他はざっくりいえば, IP ヘッダ, ICMP データをそれぞれ RFC に記述のとおり並べたり, 読んだりすればよい.
結局, 詳細は下記リポジトリを参照されたい.

### 実装

実装は, 次のリポジトリで管理している. 
冒頭でも述べた通り, リポジトリ内にある Vagrantfile の環境上で動作を確認している.

<p style="text-align: center;">
<i class="fab fa-github" style="font-size: large; margin-right: 5px;"></i>
<a href="https://github.com/falgon/network-basal">falgon/network-basal - Low layer network packet utilities</a>
</p>


これには先に述べた arp-exe のほかに, 実行可能なアプリケーションとして,
ping-exe と ping-exe2 が含まれている. ping-exe2 が本エントリで述べたように,
イーサネットフレームを丸々扱い, ICMP Echo の送出および ICMP Echo Reply の受信を行う.

```sh
$ sudo stack --allow-different-user exec ping-exe2 -- --help
usage: ping-exe [-c count] [-t timeout] [-i wait] host
$ sudo stack --allow-different-user exec ping-exe2 -- -c 1 8.8.8.8
PING 8.8.8.8: 56 data bytes
64 bytes from google-public-dns-a.google.com: icmp_seq=1 ttl=63 time=11.432482s

--- ping statics ---
1 packets transmitted, 1 received, 0% packet loss
```

一方, ping-exe は`PF_INET`で開いたソケットを利用して, 
つまり ICMP データのみを構築して ICMP Echo の送出および ICMP Echo Reply の受信を行う.
冒頭で述べたような立場からすれば, これの実装に対しては特に意味はないのであるが,
一応, 同様にして動くということをみるために作ってみた.

### 感想

Haskell でまとまったプログラムを書いたことは, 今回と[エルガマル暗号の実装](https://falgon.github.io/roki.log/posts/2018/%207月/13/elgamalEncryption/)以外ではあまりなかったため, 
学びがあった. ネットワークに関しても, やはり実装することでかなり整理がついたように思える.
リンクレイヤーも慣れてきた感じがあるので, 気が向き次第, 今度はルーターとかを作れればよいな等と思っている.


[^1]: これに関するコンパクトで的を得た回答: [ARP request outside of LAN; Target machine or router response? - Stack Exchange](https://networkengineering.stackexchange.com/a/6854) 2018 年 9 月 3 日アクセス.
