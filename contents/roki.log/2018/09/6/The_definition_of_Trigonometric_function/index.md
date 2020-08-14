---
title: 三角関数の公式の導出
date: 2018-09-06 00:00:00
tags: math
header-warn: この記事は, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>から移植された記事です. よって, その内容として, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>に依存した文脈が含まれている可能性があります. 予めご了承下さい.
---

[参考文献 1](#ref1) では, 高木貞治氏の書いた[解析概論](#ref2)の緒言として示されている三角関数の古典的な導入法の問題点と,
それに対する合理的な導入, 定義に関する記述があり, 興味深かったので読んでいたのだが, 
ふと高校数学 Ⅲ  の「普通な」加法定理や積和, 和積の公式, 導関数の導出などが頭から抜けていたので, 復習がてら書くことにした.
一応, このエントリで言う三角関数 \\(\cos\theta,\sin\theta\\) の定義は高校数学の範囲で言われる定義と同様であり, 次のとおりである.

<div class="m-def">
<header class="m-def-title"><p><span id="euclidean">高校数学における \\(\cos\theta, \sin\theta\\)</span></p></header>
<div class="m-def-content">
直行座標平面上の原点 \\(O(0,0)\\) を中心とする半径 \\(1\\) の円 \\(C\\) の \\(x\geq 0,y\geq 0\\) の部分を \\(C_{+}\\) としたとき,
弧度法によると, 点 \\(A(1,0)\\), \\(C_{+}\\) 上の点 \\(P(x,y)\\) を角 \\(A O P\\) が \\(\theta\ (0\lt\theta\leq\frac{\pi}{2})\\) となるようにとれば,
孤 \\(A P\\) の長さは 角 \\(A O P\\) そのもの, すなわち \\(\theta\\) である. このとき \\(x=\cos\theta,y=\sin\theta\\) である.
</div>
</div>

よくよく考えてみれば, [この定義](#hs_trignometric)では,
孤 \\(A P\\) の長さおよび実数 \\(0\lt\theta\leq\frac{\pi}{2}\\) に対し孤 \\(A P\\) の長さが \\(\theta\\) となる \\(C_{+}\\) 上の点 
\\(P\\) が存在することについて, 特に説明しておらず, 定義としては不十分な点があることが考えられる.
[参考文献 1](#ref1) にはこの問題に対する考察が綴られており, 読みやすい文体で書かれているので興味があれば読んでみることを勧める.
本エントリはそのような意味で, 特に面白みもなくただ単に高校数学 Ⅲ までの三角関数の内容を復習しているだけのものとなっているので, その点は悪しからず.

<!--more-->

## 加法定理

この間で余弦定理を暗に認めたものとして利用する.

<div style="text-align:center;">
<a title="三村周平 [CC BY-SA 3.0 (https://creativecommons.org/licenses/by-sa/3.0)], via Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File:%E5%8A%A0%E6%B3%95%E5%AE%9A%E7%90%86.png"><img width="330" alt="加法定理" src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/07/%E5%8A%A0%E6%B3%95%E5%AE%9A%E7%90%86.png/512px-%E5%8A%A0%E6%B3%95%E5%AE%9A%E7%90%86.png"></a>
</div>

単位円上の二点 \\(P(\cos p,\sin p),Q(\cos q,\sin q)\\) がある.
上図のように, 原点 \\(O\\) に対し, \\(O P\\) と \\(x\\) 軸の成す角を \\(p\\), 
\\(O Q\\) と \\(x\\) 軸の成す角を \\(q\\) とする. 線分 \\(P Q\\) の長さを座標成分で表すと, 

\\[\begin{aligned}
P Q^2&=&(\cos q-\cos p)^2+(\sin q-\sin p)^2\\
&=&\cos^2 q-2\cos q\cos p+\cos^2 p+\sin^2 q-2\sin q\sin p+\sin^2 p\\
&=&(\sin^2 p+\cos^2 p)+(\sin^2 q+\cos^2 q)-2\cos q\cos p-2\sin q\sin p\\
&=&2-2(\sin p\sin q+\cos p\cos q)\tag{1}
\end{aligned}\\]

また, 余弦定理より

\\[\begin{aligned}
P Q^2&=&O P^2+O Q^2-2 O P\cdot O Q\cos(p-q)\\
&=&1^2+1^2-2\cdot 1\cdot 1\cdot \cos(p-q)\\
&=&2-2\cos(p-q)\tag{2}
\end{aligned}\\]

\\((1),(2)\\) より
\\[2-2\cos(p-q)=2-2(\cos p\cos q+\sin p\sin q)\leftrightarrow 
\cos(p-q)=\cos p\cos q+\sin p\sin q\tag{3}\\]

ここで, \\((3)\\) の \\(q\\) を \\(q+\frac{\pi}{2}\\) とすると, 三角関数の定義より

\\[
\cos{p-(q+\frac{\pi}{2})}=\cos p\cos(q+\frac{\pi}{2})+\sin p\sin(q+\frac{\pi}{2})\leftrightarrow\sin(p-q)=\sin p\cos q-\cos p\sin q
\\]

\\(q=-q\\) とおくと \\[\sin(p+q)=\sin p\cos q+\cos p\sin q\tag{4}\\]

### 三角関数の導関数

まず \\(f(x)=\sin x\\) の導関数 \\(f'(x)\\) について, 導関数の定義より

\\[\begin{aligned}
f'(x)&=&\lim_{h\to 0}\frac{f(x+h)-f(x)}{h}\\
&=&\lim_{h\to 0}\frac{\sin(x+h)-\sin x}{h}\\
&=&\lim_{h\to 0}\frac{\sin x\cos h+\cos x\sin h-\sin x}{h}\ \because{\rm 加法定理}\ (4)\ {\rm より}\tag{5}
\end{aligned}
\\]

\\[
\begin{aligned}
(5)&=&\lim_{h\to 0}\frac{\sin x(\cos h-1)+\cos x\sin h}{h}\\
&=&\lim_{h\to 0}\left\{\frac{\sin x(\cos h-1)}{h}+\frac{\cos x\sin h}{h}\right\}\\ 
&=&\lim_{h\to 0}(\sin x\underbrace{\frac{\cos h - 1}{h}}_{A}+\cos x\frac{\sin h}{h})\tag{6}
\end{aligned}
\\]

項 \\(A\\) について

\\[
\begin{aligned}
\frac{\cos h-1}{h}\cdot\frac{\cos h+1}{\cos h+1}&=&\frac{\cos^2h-1}{h(\cos h+1)}\\
&=&\frac{-\sin^2 h}{h(\cos h+1)}\ \because\sin^2+\cos^2=1\\
&=&\frac{-\sin h\cdot\sin h}{h(\cos h+1)}\cdot\frac{h}{h}\\
&=&-\frac{\sin h}{h}\cdot\frac{\sin h}{h}\cdot\frac{h}{\cos h+1}
\end{aligned}
\\]

ここで, 
\\(\displaystyle\lim_{h\to 0}-\frac{\sin h}{h}\cdot\frac{\sin h}{h}\cdot\frac{h}{\cos h+1}=0\\) 
だから,
\\((6)\\) より \\[f'(x)=\sin x\cdot 0+\cos x\cdot 1=\cos x\tag{7}\\]
次に \\(f(x)=\cos x\\) の導関数 \\(f'(x)\\) について, 導関数の定義より

\\[
\begin{aligned}
f'(x)&=&\lim_{h\to 0}\frac{f(x+h)-f(x)}{h}\\
&=&\lim_{h\to 0}\frac{\cos(x+h)-\cos x}{h}\\
&=&\lim_{h\to 0}\frac{\cos x\cos h-\sin x\sin h-\cos x}{h}\ \because{\rm 加法定理}\ (3)\ {\rm より}\tag{8}\end{aligned}\\]

\\[
\begin{aligned}
(8)&=&\lim_{h\to 0}\frac{\cos x(\cos h-1)-\sin x\sin h}{h}\\
&=&\lim_{h\to 0}\left\{\frac{\cos x(\cos h-1)}{h}-\frac{\sin x\sin h}{h}\right\}\\ 
&=&\lim_{h\to 0}(\cos x\frac{\cos h - 1}{h}-\sin x\frac{\sin h}{h})\\
&=&\cos x\cdot 0-\sin x\cdot 1\\
&=&-\sin x\tag{9}
\end{aligned}
\\]

次に \\(f(x)=\tan x\\) の導関数 \\(f'(x)\\) について,
これは \\(f'(x)=(\tan x)'=(\frac{\sin x}{\cos x})'\\) だから

\\[
\begin{aligned}
f'(x)&=&(\frac{\sin x}{\cos x})'\\
&=&\frac{(\sin x)'\cos x-\sin x(\cos x)'}{\cos^2 x}\\
&=&\frac{\cos x\cos x-\sin x(-\sin x)}{\cos^2 x}\ \because (7),(9)\ {\rm より}\\
&=&\frac{\cos^2 x+\sin^2 x}{\cos^2 x}\\
&=&\frac{1}{\cos^2 x}
\end{aligned}
\\]

最後に \\(f(x)=\frac{1}{\tan x}\\) の導関数 \\(f'(x)\\) について,
これは \\(f'(x)=\frac{1}{\tan x}=(\frac{\cos x}{\sin x})'\\) だから

\\[
\begin{aligned}
f'(x)&=&(\frac{\cos x}{\sin x})'\\
&=&\frac{(\cos x)'\sin x-\cos x(\sin x)'}{\sin^2 x}\\
&=&\frac{-\sin x\sin x-\cos x\cos x}{\sin^2 x}\ \because (7),(9)\ {\rm より}\\
&=&-\frac{\sin^2x+\cos^2x}{\sin^2x}\\
&=&-\frac{1}{\sin^2x}
\end{aligned}
\\]

## 和積の公式を用いた方法

\\((5),(8)\\) の部分では加法定理を用いたが, 
加法定理より導出できる和積の公式を用いても同様にして導出できる.
\\((4)\\) より

\\[
\sin(p+q)=\sin p\sin q+\cos p\sin q\tag{10}
\\]
\\[
\sin(p-q)=\sin p\sin q-\cos p\sin q\tag{11}
\\]

\\((10)+(11)\\) より 
\\[\sin(p+q)+\sin(p-q)=2\sin p\cos q\leftrightarrow 
\sin p\cos q=\frac{\sin(p+q)+\sin(p-q)}{2}\tag{12}\\]
また, \\((3)\\) より

\\[
\cos(p+q)=\cos p\cos q-\sin p\sin q\tag{13}
\\]
\\[
\cos(p-q)=\cos p\cos q+\sin p\sin q\tag{14}
\\]

\\((13)-(14)\\) より 
\\[\cos(p+q)-\cos(p-q)=-2\sin p\sin q\leftrightarrow 
\sin p\sin q=-\frac{\cos(p+q)-\cos(p-q)}{2}\tag{15}\\]

\\((12),(15)\\) は積和の公式といわれる 
(あともう 1 つ積和の公式と言われるものがあるが, 今回は利用しないので省略). 
ここで, \\((12)\\) に対し \\(p=\frac{x-y}{2},q=\frac{x+y}{2}\\) とすると,

\\[\begin{aligned}
\sin\frac{x-y}{2}\cos\frac{x+y}{2}&=&\frac{\sin(\frac{x-y}{2}+\frac{x+y}{2})+\sin(\frac{x-y}{2}-\frac{x+y}{2})}{2}\\
&=&\frac{\sin x-\sin y}{2}
\end{aligned}
\\]

ゆえに
\\[\sin x-\sin y=2\cos\frac{x+y}{2}\sin\frac{x-y}{2}\tag{16}\\]
また \\(\\) に対し \\(p=\frac{x+y}{2},q=\frac{x-y}{2}\\) とすると,

\\[
\begin{aligned}
\sin\frac{x+y}{2}\sin\frac{x-y}{2}&=&-\frac{\cos(\frac{x+y}{2}+\frac{x-y}{2})-\cos(\frac{x+y}{2}-\frac{x-y}{2})}{2}\\
&=&-\frac{\cos x-\cos y}{2}
\end{aligned}
\\]

ゆえに

\\[\cos x-\cos y=-2\sin\frac{x+y}{2}\sin\frac{x-y}{2}\tag{17}\\]
\\((16),(17)\\) が和積の公式である 
(あともう 2 つ和積の公式と言われるものがあるが, 今回は利用しないので省略).
\\((16)\\) をつかって \\(\displaystyle f'(x)=\lim_{h\to 0}\frac{\sin(x+h)-\sin x}{h}\\) を変形すると,

\\[\begin{aligned}
f'(x)&=&\lim_{h\to 0}\frac{\sin(x+h)-\sin x}{h}\\
&=&\lim_{h\to 0}\frac{2\cos(\frac{2x+h}{2})\sin\frac{h}{2}}{h}\ \because (16)\\
&=&\lim_{h\to 0}\frac{\cos(\frac{2x+h}{2})\sin\frac{h}{2}}{\frac{h}{2}}\\
&=&\cos(\frac{2x}{2})\\
&=&\cos x
\end{aligned}\\]

と \\((7)\\) と同様の結果が得られる. 
また, \\((17)\\) をつかって 
\\(\displaystyle f'(x)=\lim_{h\to 0}\frac{\cos(x+h)-\cos x}{h}\\) を変形すると,

\\[\begin{aligned}
f'(x)&=&\lim_{h\to 0}\frac{\cos(x+h)-\cos x}{h}\\
&=&\lim_{h\to 0}\frac{-2\sin(\frac{2x+h}{2})\sin\frac{h}{2}}{h}\ \because (17)\\
&=&\lim_{h\to 0}-\frac{\sin(\frac{2x+h}{2})\sin\frac{h}{2}}{\frac{h}{2}}\\
&=&-\sin(\frac{2x}{2})\\
&=&-\sin x
\end{aligned}
\\]

と \\((9)\\) と同様の結果が得られる.

## 参考文献

1. 『<a id="ref1" href="http://www.ms.u-tokyo.ac.jp/~t-saito/jd/%E4%B8%89%E8%A7%92%E9%96%A2%E6%95%B0.pdf">三角関数とは何か</a>』2018 年 9 月 6 日アクセス.
2. <a id="ref2" class="disabled">高木貞治 (1983) 『解析概論』岩波書店</a>
