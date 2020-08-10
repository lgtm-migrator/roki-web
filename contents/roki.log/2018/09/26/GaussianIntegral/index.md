---
title: ガウス積分の公式とその証明
date: 2018-09-26 00:00:00
tags: math, Python
---

当ブログ内でガウス積分(オイラー＝ポアソン積分)の公式を用いる際に self-contained でリファレンスを張るためと, 
個人的な学習の記録として, 本エントリにてガウス積分の公式とその証明について書く[^1]$.$
筆者自身にとっての分かりやすさを優先しているため, 若干冗長的な記述があるかもしれない点に注意.

<div class="panel panel-default">
  <div class="panel-heading theo"><a name="GaussianIntegral" class="disabled">ガウス積分の公式</a></div>
  <div class="panel-body" style="overflow:scroll">
  \\(x\in\mathbb{R}\\) のとき \\[\displaystyle\int_{-\infty}^{\infty}e^{-x^2}dx=\sqrt{\pi}\\]
  </div>
</div>

**証明**:
<br>

\\[I=\displaystyle\int_{-\infty}^{\infty}e^{-x^2}dx\\] とおく. 
ここで, 最終的に \\(\pi\\) を出現させるために, 直交座標系から極座標系への移行を行いたい.
そのために, まず二乗して

\\[I^2=\displaystyle(\int_{-\infty}^{\infty}e^{-x^2}dx)^2=
(\int_{-\infty}^{\infty}e^{-x^2}dx)\cdot(\int_{-\infty}^{\infty}e^{-x^2}dx)
\\]

文字を変えても積分値に変わりはないから

\\[I^2=(\int_{-\infty}^{\infty}e^{-x^2}dx)\cdot(\int_{-\infty}^{\infty}e^{-y^2}dy)=
\int_{-\infty}^{\infty}\int_{-\infty}^{\infty}e^{-(x^2+y^2)}dxdy\\]

\\(x=r\cos\theta,\ y=r\sin\theta, dx\ dy=rdrd\theta\\) とし[^2]

\begin{aligned}
I^2&=&\int_{0}^{2\pi}\int_{0}^{\infty}e^{-r^2}rdrd\theta \\
&=&\int_{0}^{2\pi}d\theta\int_{0}^{\infty}re^{-r^2}dr \\
&=&2\pi\left[\dfrac{1}{2}e^{-r^2}\right]^{\infty}_{0} \\
&=&\pi
\end{aligned}
もともと \\(I\\) は被積分関数の関数形であり, 
定義域は \\(I > 0\\) だから, \\(I=\sqrt{\pi}\\). \\(\square\\)

2 乗して \\(x^2+y^2=r^2\\)[^3] を出現させ, 極座標での表現を開始する流れは, 胸熱であった.
さて, 以下はガウス積分の公式に関連した, いくつかの等式について示すこととする.

<div class="panel panel-default">
  <div class="panel-heading theo"><a name="SimilarGaussianIntegral1" class="disabled">ガウス積分の類似形 1</a></div>
  <div class="panel-body" style="overflow:scroll">
  \\(x\in\mathbb{R}, a\in\mathbb{R}^{+}\\) のとき, \\[\displaystyle\int_{-\infty}^{\infty}e^{-ax^2}dx=\sqrt{\dfrac{\pi}{a}}\\]
  </div>
</div>

**証明**:<br>

\\(y=\sqrt{a}x, dy=\sqrt{a}dx\\) とし,

\\[\int_{-\infty}^{\infty}e^{-ax^2}dx=\int_{-\infty}^{\infty}e^{-y^2}\cdot\dfrac{1}{\sqrt{a}}dy=\dfrac{1}{\sqrt{a}}\int_{-\infty}^{\infty}e^{-y^2}dy\tag{1}\\]
\\(\\) の最右辺をみると[ガウス積分の公式](#GaussianIntegral)と全く同じなので, \\(=\sqrt{\dfrac{\pi}{a}}\\). \\(\square\\)

<div class="panel panel-default">
  <div class="panel-heading theo"><a name="SimilarGaussianIntegral2" class="disabled">ガウス積分の類似形 2</a></div>
  <div class="panel-body" style="overflow:scroll">
  \\(x\in\mathbb{R}, a\in\mathbb{R}^{+}\\) のとき, \\[\displaystyle\int_{0}^{\infty}e^{-ax^2}dx=\dfrac{1}{2}\sqrt{\dfrac{\pi}{a}}\\]
  </div>
</div>

**証明**:<br>

単に[ガウス積分の類似形 1](#SimilarGaussianIntegral1)の半分の領域となるだけなので,
\\(\displaystyle\int_{0}^{\infty}e^{-ax^2}dx=\dfrac{1}{2}\sqrt{\dfrac{\pi}{a}}\\). \\(\square\\)

### 参考文献

<ul>
<li>「<a name="ref1" href="https://mathtrain.jp/gauss">ガウス積分の公式の 2 通りの証明</a>」 2018 年 9 月 26 日アクセス.</li>
<li>「<a href="http://www.chem.tsukuba.ac.jp/kazuya/kazuya/AppC.pdf">C. 極座標</a>」 2018 年 9 月 26 日アクセス.</li>
</ul>

[^1]: 証明内では, [フビニの定理](https://ja.wikipedia.org/wiki/%E3%83%95%E3%83%93%E3%83%8B%E3%81%AE%E5%AE%9A%E7%90%86)を暗黙に使っている. 恥ずかしながら, 筆者は測度論について全くの素人であるので, これを暗に用いることはあまりよくないと思うのだが, これが[<i>シグマの二重和が分解できることの一般形</i>](#ref1)であると理解して, 今回はこれを用いた.
[^2]: 補足: 極座標系において, \\(\theta\\) の変域は \\([0,2\pi]\\), \\(r\\) の変域は \\([0,\infty]\\) である. また, 極座標での微小面積は \\(drd\theta\\) ではなく \\(rdrd\theta\\) であることに注意. これについては, 後日のエントリ, [ヤコビアン](/roki.log/posts/2018/10/04/jacobian/)にて取り扱っている.
[^3]: 一応書いておくと, この裏付けは三平方の定理より \\(\cos^2+\sin^2=1\\).
