---
title: Haskell で D-Bus から systemd unit を制御する
date: 2018-08-17 00:00:00
tags: Haskell
header-warn: この記事は, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>から移植された記事です. よって, その内容として, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>に依存した文脈が含まれている可能性があります. 予めご了承下さい.
---

D-Bus とはメッセージバスシステムであり, アプリケーション間で互いにやりとりを行うためのプロセス間通信実装の 1 つである.
システムデーモン(新しいハードウェアデバイスの追加やプリンタキューの変更などのイベント等)と,
ユーザー単位のログインセッションデーモン(ユーザーアプリケーション間の一般的なIPC)を提供する[^1].

現代的な Linux カーネルの init プロセスにて起動される systemd デーモンおよびその補助デーモンは,
D-Bus にいくつかの API を公開している. 
私の観測範囲内において, C や Python, Go 等でこれらを利用する例はそこそこ見たことがあるのだが, 
Haskell での取り組みは一切見たことがなかったので, 少々これらで遊んで見た日記として本エントリに記す.

<!--more-->

## D-Bus API の確認

実行環境は, 次の通りである.

```sh
$ uname -a
Linux vagrant 4.15.0-20-generic #21-Ubuntu SMP Tue Apr 24 06:16:15 UTC 2018 x86_64 x86_64 x86_64 GNU/Linux
$ systemd --version
systemd 237
+PAM +AUDIT +SELINUX +IMA +APPARMOR +SMACK +SYSVINIT +UTMP +LIBCRYPTSETUP +GCRYPT +GNUTLS +ACL +XZ +LZ4 +SECCOMP +BLKID +ELFUTILS +KMOD -IDN2 +IDN -PCRE2 default-hierarchy=hybrid
```

D-Bus に公開されている API を利用する際は, 
とくに高度なラッパーライブラリを用いないような場合においては,
`dbus-send`あるいは`gdbus`等で全体の構造, 
インタフェース, メソッドおよびフィールドメンバーを確認するとよい.

```sh
$ function syst(){ r=$(gdbus introspect --system --dest org.freedesktop.systemd1 --object-path /org/freedesktop/systemd1); echo ${r} | head -n $1 && echo "More than" $(($(echo ${r} | wc -l) - $1)) "lines..."; }
$ syst 10
node /org/freedesktop/systemd1 {
  interface org.freedesktop.DBus.Peer {
    methods:
      Ping();
      GetMachineId(out s machine_uuid);
    signals:
    properties:
  };
  interface org.freedesktop.DBus.Introspectable {
    methods:
More than 397 lines...
```

このインタフェース表記の意味するところに関する詳細は,
D-bus 仕様の Type System セクション[^2]に記載されている.

## D-Bus の Haskell バインドの利用

[dbus](http://hackage.Haskell.org/package/dbus) が利用できる.
例えば, 以下に示す`StartUnit`, `StopUnit`は,

```sh
$ gdbus introspect --system --dest org.freedesktop.systemd1 --object-path /org/freedesktop/systemd1 | grep -e StartUnit -e StopUnit -w -A 2
      StartUnit(in  s arg_0,
                in  s arg_1,
                out o arg_2);
--
      StopUnit(in  s arg_0,
               in  s arg_1,
               out o arg_2);
```

次のようにして呼び出せる.

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import DBus
import DBus.Client
import Data.Int (Int32)

type Unit = String
type Mode = String
type SignalNum = Int32

systemdObjectPath :: ObjectPath
systemdObjectPath = objectPath_ "/org/freedesktop/systemd1"

systemdInterfaceName :: InterfaceName
systemdInterfaceName = interfaceName_ "org.freedesktop.systemd1.Manager"

systemdDestination :: BusName
systemdDestination = busName_ "org.freedesktop.systemd1"

methodSub :: String -> MethodCall
methodSub = methodCall systemdObjectPath systemdInterfaceName . memberName_

systemdCall :: Client -> MethodCall -> IO [Variant]
systemdCall = (.) (fmap methodReturnBody) . call_

controlUnit :: String -> Client -> Unit -> Mode -> IO [Variant]
controlUnit med cli unit mode = systemdCall cli (methodSub med) {
    methodCallDestination = Just systemdDestination,
    methodCallBody = map toVariant [unit, mode]
}

startUnit :: Client -> Unit -> Mode -> IO [Variant]
startUnit = controlUnit "StartUnit"

stopUnit :: Client -> Unit -> Mode -> IO [Variant]
stopUnit = controlUnit "StopUnit"
```

以下に示す`ListUnitsByNames`[^3]は,

```sh
$ gdbus introspect --system --dest org.freedesktop.systemd1 --object-path /org/freedesktop/systemd1 | grep ListUnitsByNames -w -A 1
      ListUnitsByNames(in  as arg_0,
                       out a(ssssssouso) arg_1);
```

次のようにして呼び出せる.

```Haskell
listUnitsByNames :: IsValue a => Client -> [a] -> IO [Variant]
listUnitsByNames cli var = systemdCall cli (methodSub "ListUnitsByNames") {
    methodCallDestination = Just systemdDestination,
    methodCallBody = [toVariant var]
}
```

動作確認のためのユニットを適当に置いておく[^4].

```sh
$ mkdir -p /opt/writehello/bin
$ sudo sh -c "echo \"#\!/bin/bash\nwhile :\ndo\n\tsleep 3\n\techo \"hello\"\ndone\"" > writehello.sh
$ sudo chmod +x /opt/writehello/bin/writehello.sh
$ sudo sh -c "echo \"[Unit]\nDescription = hello daemon\nConditionPathExists = /opt/writehello/bin/writehello.sh\n\n[Service]\nExecStart = /opt/writehello/bin/writehello.sh\nRestart = always\nType = simple\n\n[Install]\nWantedBy = multi-user.target\"" > /etc/systemd/system/writehello.service
$ sudo chmod -x /etc/systemd/system/writehello.service
$ sudo chmod o-w /etc/systemd/system/writehello.service
$ sudo systemd-analyze verify /etc/systemd/system/writehello.service
Attempted to remove disk file system, and we can't allow that.
$ sudo systemctl daemon-reload
$ sudo systemctl start writehello.service && journalctl -f -u writehello.service & sleep 10 && sudo kill $! && sudo systemctl stop writehello.service
[1] 2001
-- Logs begin at Fri 2018-08-17 16:19:05 UTC. --
Aug 17 16:19:13 vagrant systemd[1]: Started hello daemon.
Aug 17 16:19:16 vagrant writehello.sh[1989]: hello
Aug 17 16:19:19 vagrant writehello.sh[1989]: hello
Aug 17 16:19:22 vagrant writehello.sh[1989]: hello
```
 
先の関数らから writehello.service ユニットを制御する.

```Haskell
module Main where

import DBus.Client (connectSystem)
import System.Environment (getArgs)
import Control.Monad (mapM_, (<=<))

main :: IO ()
main = do
    client <- connectSystem
    args <- getArgs
    mapM_ (print <=< flip (startUnit client) "replace") args
    print =<< listUnitsByNames client args
```

引数に writehello.service を指定してスーパーユーザで実行すると, 次のような出力が得られる.

```sh
[Variant (ObjectPath "/org/freedesktop/systemd1/job/1053")]
[Variant [("writehello.service", "hello daemon", "loaded", "active", "running", "", ObjectPath "/org/freedesktop/systemd1/unit/writehello_2eservice", 0, "", ObjectPath "/")]]
```

停止も忘れずに.

```Haskell
mapM_ (print <=< flip (stopUnit client) "replace") args 
```

なお, 本エントリにおける一連の実装とその他の 
systemd D-Bus API を利用したいくつかの snippets を下記リポジトリにて管理している.

<div class="box has-text-centered is-shadowless">
<i class="fab fa-github mr-2"></i>
[r0ki/systemdplhs - Snippets collection that controls systemd from D-Bus with Haskell.](https://bitbucket.org/r0ki/systemdplhs/src/master/)
</div>

[^1]: 説明は[公式ページ](https://www.freedesktop.org/wiki/Software/dbus/#index1h1)から.
[^2]: "D-Bus Specification", <https://dbus.freedesktop.org/doc/dbus-specification.html#type-system> 2018 年 8 月 17 日アクセス.
[^3]: Note: `ListUnitsByName` は systemd v230 以上を要する.
[^4]: `systemd-analyze verify`の結果で, `Attempted to remove disk file system, and we can't allow that.`というメッセージが出力されているが, これは systemd 237-4 および 238 でのバグ([#8592](https://github.com/systemd/systemd/issues/8592))との報告がある.
