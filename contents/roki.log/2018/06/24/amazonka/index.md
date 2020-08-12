---
title: amazonka で EC2 インスタンスの操作
date: 2018-06-24 10:45:00
tags: Haskell, Networking
---

以前のエントリ[^1]では, AWS の API を golang から叩くために golang の言語機能を一通り触った.
自分は元々よく C++ を書いていたので golang で叩く前にまず C++ から適当に叩いてみていたのだが,
ふと Haskell 用の AWS のライブラリの存在を知り, ひとまず触ってみたので, 記録と紹介を兼ねて書き残してみる.
執筆時現時点では, 公式が提供する AWS SDK は awscli を除き
C++, Go, Java, javascript, .NET, PHP, Python, Ruby [^2]となっており, 
amazonka[^3] は非公式の Haskell 用 AWS SDK である.
基本的には, まず amazonka を導入した後, 必要となる AWS サービスに該当する amazonka-\*\* を導入する. 今回は, ec2 インスタンスの立ち上げ, 停止を行なったので, amazonka-ec2[^4] を利用した.
amazonka は全体のデータのやり取りに Lens を多様しており, 利用者はこの恩恵を傍受できる.

早速であるが, くだらないサンプルを次に示す.
とくにいま ec2 インスタンス上で何かしたいことはないので, 
東京リージョンでインスタンスを立ち上げて, その直後に落とすこととする[^5]. 

```Haskell
module Main where
import qualified Network.AWS as AWS
import qualified Network.AWS.Data as AWSD
import qualified Network.AWS.EC2 as EC2
import System.IO (stdout)
import Control.Lens

instanceIds :: String
instanceIds = "i-********"

main :: IO ()
main = do
    let t0 = EC2.startInstances & EC2.sInstanceIds .~ [AWSD.toText instanceIds]
    let t1 = EC2.stopInstances & EC2.siInstanceIds .~ [AWSD.toText instanceIds]
    env <- AWS.newEnv (AWS.FromFile (AWSD.toText "default") "/path/to/credentials")
    lgr <- AWS.newLogger AWS.Debug stdout
    res0 <- AWS.runResourceT $ AWS.runAWS (env & AWS.envLogger .~ lgr) $ AWS.within AWS.Tokyo $ AWS.send t0
    res1 <- AWS.runResourceT $ AWS.runAWS (env & AWS.envLogger .~ lgr) $ AWS.within AWS.Tokyo $ AWS.send t1
    print res0
    print res1
```

本家の各 SDK と同じように, 必要なサービスごとにライブラリがそれぞれ独立しているので,
基本的にまずは [AWS CLI Command Reference](https://docs.aws.amazon.com/ja_jp/cli/latest/index.html) をみてから, その操作を行うのに必要とするライブラリを amazonka から選んで導入することで, 実際に動くまでを円滑に進めることができるだろう.

### 参照
* <a id="ref1">"amazonka: Comprehensive Amazon Web Services SDK."</a> <http://hackage.Haskell.org/package/amazonka> 2018 年 6 月 24 日アクセス.
* <a id="ref2">"amazonka-ec2: Amazon Elastic Compute Cloud SDK."</a> <http://hackage.Haskell.org/package/amazonka-ec2-1.6.0> 2018 年 6 月 24 日アクセス.
* "Network.AWS" <http://hackage.Haskell.org/package/amazonka-1.6.0/docs/Network-AWS.html> 2018 年 6 月 24 日アクセス
* "Network.AWS.EC2" <http://hackage.Haskell.org/package/amazonka-ec2-1.6.0/docs/Network-AWS-EC2.html> 2018 年 6 月 24 日アクセス

[^1]: roki (2018)「golang 始めたてメモ」<https://falgon.github.io/roki.log/posts/2018/%206月/17/golangtrial/>
[^2]: 「AWS SDK とツール」<https://aws.amazon.com/en/getting-started/tools-sdks/> 2018 年 6 月 24 日アクセス.
[^3]: [参照](#ref1).
[^4]: [参照](#ref2).
[^5]: 前提として, AWS IAM の設定およびアクセスキーの設定が済んでいること.
