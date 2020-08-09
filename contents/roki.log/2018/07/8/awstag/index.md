---
title: EC2 のタグを SDK で操作
date: 2018-07-08 12:50:00
tags: golang, AWS, Networking
---

ついこの間に golang を初めて触り[^1],
大学の授業過程で AWS EC2 を使っていて, 
そのタグ機能を色々機械的に操作できれば便利だななどと思い, 
golang でなにか作る良い機会であるような気がしたので,
取り敢えず AWS EC2 のタグ機能に関する操作一式をそれなりに揃えたコマンドラインツールを作ってみた. 実装部分にかかった時間は 30 分程度であった. 
golang に慣れない身でも, API, 標準ライブラリ, 
ドキュメントが整備されているおかげでサクサクと動くところまで持っていくことができた.

<p style="text-align: center;">
<i class="fab fa-github" style="font-size: large; margin-right: 5px;"></i>
<a href="https://github.com/falgon/goec2tag">falgon/goec2tag: Instant CLI tool for mechanically manipulating AWS EC2 tags</a>
</p>

ほぼリポジトリにおいてある README のままであるが.
```sh
$ ./dst/main --help
Usage of ./dst/q3:
  -addT
        Give the tag to the instance.
  -endpoint string
        Endpoint.
  -filter string
        This flag is used in conjunction with the showtags flag to filter tags by describing filter statements.
        [Example]:
                 ... -filter 'name:resource-id,values:i-xxxxxxxx i-yyyyyyyy'
  -instances string
        Instance id or instance tag name.
  -region string
        Region name (default "ap-northeast-1")
  -rmT
        Remove tag from instance.
  -showtags
        DescribeTags API operation for EC2.
         Describes one or more of the tags for your EC2 resources. filter=...
  -tags string
        Tag Key(Use Key=) and Tag Value(Use Value=)
        [Example]:
                 ... -tags='Key=foo,Value=bar Key=hoge,Value=piyo...'
$ ./dst/main -showtags
...

$ ./dst/main -showtags -filter "name:resource-id,values:i-xxxxxxxxxxxxxxxxx" # filtering
...

$ ./dst/main -instances=i-xxxxxxxxxxxxxxxxx -tags='Key=test,Value=hoge' -addT # adding tag
...

$ ./dst/q3 -instances=i-xxxxxxxxxxxxxxxxx -tags='Key=test,Value=hoge' -rmT # remove tag
...
```
まあ, これは言ってしまえば AWS SDK for go から単に API を叩いただけで, 
実際に使うのであれば正直 awscli で事足りるから, 存在意義は全くないと思う.
個人的な収穫としては, 使い慣れていない言語のパラダイムをいくつか知ることはできた.
とくに golang 標準の flags パッケージ, これは中々便利に感じた.
自分はもともと C++ ばかり書いていたので, 
こういうことを C++ で書きたいときは, 
Boost.Program_options とかの非標準ライブラリを使うことになるか, 
自作するかになるだろうなあなどと思った. C++20 に期待(?).


[^1]: roki (2018) 「roki (2018)「golang 始めたてメモ」<https://falgon.github.io/roki.log/posts/2018/%206月/17/golangtrial/>
