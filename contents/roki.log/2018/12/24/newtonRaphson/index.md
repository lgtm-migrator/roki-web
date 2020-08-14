---
title: ニュートン法の視覚化
date: 2018-12-24 16:20:00
tags: math, javascript
header-warn: この記事は, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>から移植された記事です. よって, その内容として, <a href="https://falgon.github.io/roki.log/">旧ブログ</a>に依存した文脈が含まれている可能性があります. 予めご了承下さい.
js: newtonRaphson.js
---

久しぶりにまた[^1]なにか d3.js で視覚化してみたくなったのだが, 
このエントリがポストされる次の日は[アイザック・ニュートン](https://ja.wikipedia.org/wiki/%E3%82%A2%E3%82%A4%E3%82%B6%E3%83%83%E3%82%AF%E3%83%BB%E3%83%8B%E3%83%A5%E3%83%BC%E3%83%88%E3%83%B3)の誕生日らしいので, 今回はニュートン法(Newton Raphson 法)を視覚化してみることにした.
早速であるが以下がその成果物である[^2]. 
$f(x)=0$ となる関数 $f(x)$ とその導関数 \\(f'(x)\\) 及びニュートン法の初期値を受け付け, 
実行をクリックすると関数とニュートン法の計算過程における接線がプロットされる.
デフォルトでは, \\(\sqrt{2}\\) の値を計算するように設定してある.
入力された関数を元に数値微分をしても良かったのだが, 
なんとなく導関数を入力したかったので, そのようなことはしなかった.

<div style="width:400px; height: 310px; margin: 0 auto;" class="mb-4" id="vis"></div>
<div class="content">

<div id="success_panel" class="message is-primary" style="display: none;">
<div class="message-header">
<p>Success</p>
</div>
<div class="message-body" style="text-align: center;">
<p id="success_message"></p>
</div>
</div>

<div id="error_panel" class="message is-danger" style="display: none;">
<div class="message-header">
<p>Fail</p>
</div>
<div class="message-body" style="text-align: center;">
<p id="error_message"></p>
</div>
</div>

<div id="formula" class="box has-text-centered is-shadowless">
<form class="form-inline text-center" role="form">
<div class="form-group">
<label>\\(f(x)=\\)</label>
<input id="func" type="text" class="input" style="max-width: 200px;" value="x^2-2" placeholder="e.g: x^2 - 2">
</div><br>
<div class="form-group" style="margin-top: 10px;">
<label>\\(f'(x)=\\)</label>
<input id="func_differential" type="text" class="input" style="max-width: 200px;" value="2*x" placeholder="e.g: 2 * x">
</div><br>
<div class="form-group" style="margin-top: 10px;">
<label>初期値 :</label>
<input id="initial_value" type="text" class="input" style="max-width: 200px;" value="5" placeholder="e.g: 5">
</div><br>
<input 
    style="margin-top: 10px;" 
    id="startNewtonRaph" 
    class="button is-link is-light" 
    type="button" 
    value="実行" onclick="update()" />
</form>
</div>
</div>
<br />

<!--more-->

これで終わってしまうと何とも寂しいので, 一応簡単にニュートン法について書く.
ニュートン法はとても有名な方程式の近似解を求める方法の 1 つである.
連続的関数[^3] \\(f(x)\\) が \\(f(x)=0\\) となるような \\(x\\) を求めるときに, 
予め決めた, あるいは事前の計算で求まった切片 \\(x_{n}\\) における関数 \\(f(x)\\) への接線
\\(f'(x_{n})\\) の切片 \\(x_{n+1}\\) を用いて再帰的に \\(f(x)=0\\) に近づけていくことで求める.
同手法は非常に単純ながらも効率的な近似解法であり, 同手法から発展されたいくつかの手法が考えられている.
いまニュートン法の漸化式を導出することを考える. 
計算で必要となるのは \\(f'(x_{n})\\) に対する切片 \\(x_{n+1}\\) であるから,
いま三点 \\((x_{n},0), (x_{n},f(x_{n})), (x_{n+1}, 0)\\) 
の成す直角三角形について考えると,
\\(f'(x_{n})=\frac{(x_{n})}{x_{n}-x_{n+1}}\\) より
\\[x_{n+1}=x_{n}-\frac{f(x_{n})}{f'(x_{n})}\\]
例えば \\(\sqrt{2}\\) を例にとると, \\(x^2=2\Leftrightarrow x^2-2=0\\) なので
\\(f(x)=x^2-2,f'(x)=2x\\) とおいて \\(x_{n}-\frac{x^2_{n}-2}{2x_{n}}\\) を計算すればよい.
なお \\(f'(x_{n})\\) が \\(0\\) であると, ゼロ除算になってしまうため計算することができない.
この事実は, 傾きのない場合にどちらに進んでいけば良いのかわからないという直感的な考えにおいても筋が通る.

また, \\(f(x)\\) の解が複数あるとき, 初期値によっては望まない解が導かれることがある. 
いま \\(\sqrt{2}\\) の正の解を得たいとき, 初期値を \\(-1\\) で実行してしまうと,
得られる解は \\(-\sqrt{2}\\) となる. 
直感的には決められた初期値の傾き \\(f'(x_0)\\)
によって近づいていく方向が定まってしまうからといえる.
従って同手法を適用する際は, できる限り求めたい解に近い初期値を設定するのが望ましい.

なおニュートン法は 1 変数関数のみならず多変数関数に対しても同様にして解を求めることができる.
あまり厳密には書かないが, まずは簡単のために 2 変数の関数 
\\(f_{1}(x,y),f_{2}(x,y)\\) を用いてそれを示すこととする.
\\(f_{1}(x,y),f_{2}(x,y)\\) は曲面の定義そのものであり, 
この 2 つの曲面の交わる曲線を辿っていくことで解が求まる.
1 変数のニュートン法の場合と同様に漸化式を求めていけば, 
多変数関数に対するニュートン法の漸化式も関数の値をその傾きで割る部分が出てくるが,
いま変数は複数であるので, 各方向への微小変化に対する変化量を求める必要がある. 
これを求めるには[全微分](/roki.log/2018/10/4/jacobian/#mjx-eqn-eq)をすればよいので, 結局

\\[
(x_{n+1},y_{n+1})=(x_{n},y_{n})^T-{\partial f(x_{n},y_{n})}^{-1}(f_{1}(x_{n}, y_{n}),f_{2}(x_{n},y_{n}))^T
\\]

ここで

\\[
\begin{aligned}\partial f(x,y):=
(\begin{array}{cc}
\frac{\partial f_{1}(x,y)}{\partial x} & \frac{\partial f_{1}(x,y)}{\partial y} \\
\frac{\partial f_{2}(x,y)}{\partial x} & \frac{\partial f_{2}(x,y)}{\partial y}
\end{array})
\end{aligned}
\\]

なお \\(\partial f(x,y)\\) はヤコビ行列といわれる. 
実際にコンピュータで計算する際には, \\({\partial f(x_{n},y_{n})}^{-1}(f_{1}(x_{n}, y_{n}),f_{2}(x_{n},y_{n}))^T\\) を求めるのは計算量と誤差の観点から見て困難なので,
\\(\partial f(x_{n},y_{n})\boldsymbol{a}=(f_{1}(x_{n}, y_{n}),f_{2}(x_{n},y_{n}))^T\\) を LU 分解などで解き \\((x_n,y_n)^T-\boldsymbol{a}\\) と解くことになる.

因みに, 上記で描画される接線は, 単純に ラグランジュの補完公式より
\\(y-y_{1}=\frac{y_{2}-y_{1}}{x_{2}-x_{1}}(x-x_{1})\\) を用いて描いている.
具体的には接線の関数を \\(g(x^{\star})\\) としたとき, 接点と \\(y_{2}=0\\) であるときの 2 点 \\((x,f(x)),(x-\frac{f(x)}{f'(x)},0)\\) を使って

\\[
\begin{aligned}
g(x^{\star})&=&\frac{f(x)}{\frac{f(x)}{f'(x)}}(x^{\star}-x+\frac{f(x)}{f'(x)}) \\
&=&f'(x)(x^{\star}-x+\frac{f(x)}{f'(x)}) \\
&=&f'(x)x^{\star}-f'(x)x+f(x)
\end{aligned}
\\]

と導ける. 

[^1]: 以前のエントリ, [ベジェ曲線](/roki.log/posts/2018/04/20/Bezier-curve/)では d3.js を用いて二次ベジェ曲線が描かれていく過程を書いた.
[^2]: [実装](https://github.com/falgon/roki.log/blob/gh-pages/js/newtonRaphson.js). ここで懺悔すると, 実はグラフの描画の実装についてはそこそこ手抜きをしている. 例えば解が第 1, 2 象限であるものの関数 \\(f(x)\\) の値の多くが第 3, 4 象限にあると接線が見えない. 勿論計算結果そのものは影響しない.
[^3]: 連続性に関する論法 \\(\to\\) [\\(\epsilon-\delta\\) 論法](/roki.log/2018/10/4/jacobian/#epsilonDelta-definitionOfLimit).
