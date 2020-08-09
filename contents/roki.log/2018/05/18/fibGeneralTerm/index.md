---
title: フィボナッチ数列の一般項の導出
date: 2018-05-18 19:42:00
tags: math, Haskell
---

フィボナッチ数列の漸化式を次のように置く. \\[f_{n+2} = f_{n+1} + f_n\ (n\geq 0)\\] ここで, 初項と第二項をそれぞれ \\(a_1=1, a_2=1\\) とする. 各項を \\(f_{n+2}\rightarrow c^2、f_{n+1}\rightarrow c、f_n\rightarrow 1\\) と置き換えると \\[c^2=c+1\tag{1}\\] が得られる. この解は \\(c=\dfrac{1\pm{\sqrt{5}}}{2}\\) となる. ここで, \\(\psi = \dfrac{1-\sqrt{5}}{2}, \phi = \dfrac{1+\sqrt{5}}{2}\\) と置く.
フィボナッチ数列の漸化式の特性方程式の解は \\((1)\\) の解より \\(\psi, \phi\\) であるから $$f_{n+2}=f_{n+1}+f_{n}\Leftrightarrow\begin{cases}f_{n+2}-\psi f_{n+1}=\phi(f_{n+1}-\psi f_n) \\f_{n+2}-\phi f_{n+1}=\psi(f_{n+1}-\phi f_n)\end{cases}$$
と変形できる. いま \\(b_n=f_{n+1}-\psi f_n, c_n=f_{n+1}-\phi f_n\\) と置くと次の漸化式が得られる.
$$\begin{cases}b_{n+1}=\phi b_n\\c_{n+1}=\psi c_n\end{cases}$$ 
また \\(f_1=1, f_2=1\\) より $$\begin{cases}b_1=f_2-\psi f_1=1-\psi\\ c_1=f_2-\phi f_1=1-\phi\end{cases}$$ として, 数列 \\({b_n}\\) と数列 \\({c_n}\\) の初項が求まる. 
故に, 数列 \\({b_n}\\) は初項 \\(1-\psi\\), 公比 \\(\phi\\) の等比数列であるから, \\(b_n = (1-\psi)\phi^{n-1}\\), 数列 \\({c_n}\\) は初項 \\(1-\phi\\), 公比 \\(\psi\\) の等比数列であるから, \\(c_n = (1-\phi)\psi^{n-1}\\) といえる. さらに \\(b_n, c_n\\) を上記の定義より代入すると, $$\begin{cases} \phi^n=b_n=f_{n+1}-\psi f_n\\\psi^n=c_n=f_{n+1}-\phi f_n\end{cases}$$ が得られる. 上の式から下の式を引くと \\(\phi^n-\psi^n=-\psi f_n+\phi f_n=(\phi-\psi)f_n\\) であるから, 一般項 \\(f_n\\) は \\[f_n=\dfrac{1}{\phi-\psi}( \phi^n-\psi^n)\\]
\\(\therefore\\) \\(\psi, \phi\\) を上記の定義より代入すると, 

<div class="panel panel-default">
  <div class="panel-heading def"><a class="disabled">フィボナッチ数列の一般項</a></div>
  <div class="panel-body">
\\[f_n=\dfrac{1}{\sqrt{5}}\left\{(\dfrac{1+\sqrt{5}}{2})^{n}-(\dfrac{1-\sqrt{5}}{2})^{n} \right\}\\]
  </div>
</div>

 となり, フィボナッチ数列の一般項が求まった.

確認.

```Haskell
{-# OPTIONS_GHC -Wall #-}

import System.Random (getStdRandom, randomR)
import System.IO (stderr)
import Test.HUnit (Test (TestList), (~:), (~?=), runTestText, putTextToHandle)
import Control.Monad (void)

fibGeneralTerm :: Int -> Integer
fibGeneralTerm = let phi = (1 + sqrt 5) / 2 :: Double in floor.(+0.5).(/ sqrt 5).(phi ^^)

fib :: [Integer]
fib = 0:1:zipWith (+) fib (tail fib)

mkTestList :: Int -> Int -> Int -> IO Test
mkTestList b l times = TestList <$> loop 1
    where 
        loop i 
            | i <= times = do 
                r <- getStdRandom $ randomR (b, l) 
                (:) ("fib test: " ++ show i ++ ", value: " ++ show r ~: fibGeneralTerm r ~?= fib !! r) <$> loop (succ i) 
            | otherwise = return []

main :: IO ()
main = mkTestList 0 50 5 >>= void.runTestText (putTextToHandle stderr False)
```
```
cases: 5  Tried: 5  Errors: 0  Failures: 0
```

上で導出した式の第二項の最大値は \\(\displaystyle \dfrac{1}{\sqrt{5}} \approx 0.447\\) が最大であることから, 正確な整数値を求めるのには第二項を略してしまって \\(0.5\\) を加え, 
床関数を作用させれば十分である[^1]. 上のコードでもそれを利用している. 

$$f_n=\left\lfloor \dfrac{\phi^{n}}{\sqrt{5}}+\dfrac{1}{2} \right\rfloor$$

ただし, 計算処理内で浮動小数点数による加算を用いていることから, 大きな値になればなるほど絶対誤差が生じていくことになる. 今回の実行結果も, たまたまその誤差が埋もれただけであって`fibGeneralTerm`の実行結果に対する厳密な信憑性はない.

[^1]: 参考: [ウィキペディア - フィボナッチ数](https://ja.wikipedia.org/w/index.php?title=%E3%83%95%E3%82%A3%E3%83%9C%E3%83%8A%E3%83%83%E3%83%81%E6%95%B0&oldid=68278253#.E4.B8.80.E8.88.AC.E9.A0.85)
