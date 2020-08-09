---
title: C++20 Calender and timezone library
date: 2018-03-26 16:50:00
tags: C++
---

<strong>\[<i>2018/4/16 追記</i>:</strong> 本エントリは, 元々 [P0355R5](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0355r5.html) を参考にまとめを行った記事であるが, その後 [P0355R7](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0355r7.html) で

* `sun`といった曜日を表すリテラルが全て`Sunday`, `may`といった月を表すリテラルが全て`May`といった形式に変更され, またこれらと`std::chrono::last_spec`型の`last`[^12]が`std::literals::chrono_literals`名前空間下から`std::chrono`名前空間下に移動された.
* `system_clock::to_time_t`と`system_clock::from_time_t`を Deprecated としていたが, Deprecated でなくなった.

といった他に, 細かい文面の改修や, `constexpr`がつけられるなどの変更が加えられたため, 本エントリにおいても, それに従い該当箇所を改変している(差分を示すことも考えたが, ただ見難くなるように感じたため, そのようなことはしなかった). これらの変更には対応したつもりだが, 細かい厳密な記述に関しては, やはり P0355 の最新リビジョンを追って確かめてほしい(そして, 間違った箇所があれば指摘くださると嬉しい).
<strong><i>-- 追記ここまで</i>\]</strong>

### 要旨

先週, 米国フロリダ州ジャクソンビルで ISO C++ 委員会によって, C++技術仕様(TS, 実験的機能ブランチ) と次の国際標準(IS) C++20 に関する作業が行われた.
同会議で [Reddit で紹介されているように](https://www.reddit.com/r/cpp/comments/854mu9/2018_jacksonville_iso_c_committee_reddit_trip/),
C++20 にいくつかの機能が追加された. そのうちの 1 つである, [Calender and timezone library](https://wg21.link/P0355) に関する新機能, 概要のメモ.

* 全機能の網羅性を主軸としたエントリではない(が, 独断と偏見により重要に感じた内容は結果的に網羅してしまっている部分もある)ため注意
* 以下, 特に断らない限り, 全てのコード片において`using namespace std::chrono;`, `using namespace std::chrono_literals;`がされているとする.
* 以下, 特に断らない限り, 各コードブロック以外で言及される`内容`といった記述については, 名前空間 `std::chrono`を省略する.

### 設計
以下の内容が追加される.

* `<chrono>`に対するカレンダーおよびタイムゾーンライブラリをサポートするための最小限の拡張
* Proleptic Gregorian calendar(civil calendar)
* [IANA Time Zone Database](http://www.iana.org/time-zones) を基にしたタイムゾーンライブラリ
* 分数秒, タイムゾーンの略語, UTC オフセットの完全サポートおよび`strftime`のようなフォーマッティング機能
* [IANA Time Zone Database](http://www.iana.org/time-zones) でサポートされている, 閏秒を計算するための複数のクロック

### カレンダー
本ライブラリ機能によって, 例えば 2016 年を次のように表現することができる.
```cpp
auto y = std::chrono::year{2016};
auto y = 2016y;
```
このとき, `year`は <i>partial-calendar-type (部分カレンダー型)</i> である.
他の部分カレンダー型と組み合わせることで, `year_month_day`などの <i>full-calendar-type (フルカレンダー型)</i> を生成することができる.
フルカレンダー型は, 1 日の制度を持つタイムポイントであり, 部分カレンダー型である`year`, `month`, `day` で構成される.
これらから`year_month_day`が構築されると, 内部で計算は一切起きず, 唯一起きることは, `year_month_day` が内部でそれらを格納するということだけである.
```cpp
std::chrono::year_month_day ymd1{2016y, month{5}, day{29}};
```
この場合, すべての入力がコンパイル時定数であるので, `constexpr` にすることができる. さらに, `operator/`がオーバーロードされているため, 従来の日付の構文が利用できる.
```cpp
constexpr std::chrono::year_month_day ymd1{2016y, month{5}, day{29}};
constexpr auto ymd2 = 2016y/May/29d;
constexpr auto ymd3 = May/29d/2016y;
constexpr auto ymd4 = 29d/May/2016y;
static_assert(ymd1 == ymd2);
static_assert(ymd2 == ymd3);
static_assert(ymd3 == ymd4);
static_assert(ymd1.year() == 2016y);
static_assert(ymd1.month() == May);
static_assert(ymd1.day() == 29d);
```
ここで, `May`は月を表現する`month`型のオブジェクトであり, 他に`Jan`, `Feb`, `Mar`, `Apr`, `Jun`, `Jul`, `Aug`, `Sep`, `Oct`, `Nov`, `Dec`が,
<em onclick="obj=document.getElementById('openmonth').style; obj.display=(obj.display=='none')?'block':'none';">
<a style="font-style: normal; cursor:pointer;">次のように定義される(クリックで展開).</a>
</em>
<div id="openmonth" style="display:none;clear:both;">
```cpp
namespace std::chrono {

inline constexpr chrono::month January{1};
inline constexpr chrono::month February{2};
inline constexpr chrono::month March{3};
inline constexpr chrono::month April{4};
inline constexpr chrono::month May{5};
inline constexpr chrono::month June{6};
inline constexpr chrono::month July{7};
inline constexpr chrono::month August{8};
inline constexpr chrono::month September{9};
inline constexpr chrono::month October{10};
inline constexpr chrono::month November{11};
inline constexpr chrono::month December{12};

}
```
</div>
カレンダーライブラリは, 例えばスカラ型で直接日付を指定するといったことはなく, 明示的な型指定による表現によって実現する.
なお, 部分カレンダー型(`year`, `month` に加えて`day`型)は, すべて <i>Strong ordering</i>[^1] を満たし, 加えて以下のメンバ関数をもつ.

* デフォルトコンストラクタ, `unsigned`型(`year`型のみ`int`型)の値を受け付けるコンストラクタ
* 各型の単位においてそれを前後に進める, {前|後}置{イン|デ}クリメント演算子
* 各型の単位で計算を行う`+`, `-`の二項演算子
* 二項演算子と同様の計算を行い自身に代入する`+=`, `-=` の複合代入演算子
* `unsigned`型(`year`型のみ`int`型)の値への明示的な変換(`constexpr explicit operator unsigned() const noexcept;`)
* 指定された日付が各単位で適切であるかどうかをチェックする`ok`

`year`型は, これに加えて, `is_leap`, `min`, `max` メンバ関数をもつ. `is_leap`は, 指定された年が閏年であるか判定できる. `min`, `max`は内部型の最小値と最大値を返す.
また, 上記の通り`y`といったリテラル接尾語が定義される.<br>
また`day`型は, `d`といったリテラル接尾語が定義される.

フルカレンダー型は, `sys_days`という型へ変換できる. これは, 次のように定義されている.
```cpp
constexpr year_month_day::operator sys_days() const noexcept;
```
フルカレンダー型は,`sys_days`型に変換することで,`system_clock::time_point`ファミリとの間で変換でき, これにより完全な相互運用が可能である. `sys_days` は
<em onclick="obj=document.getElementById('opensys_days').style; obj.display=(obj.display=='none')?'block':'none';">
<a style="font-style: normal; cursor:pointer;">次のように定義される(クリックで展開).</a>
</em>
<div id="opensys_days" style="display:none;clear:both;">
```cpp
using days = duration<int32_t, ratio_multiply<ratio<24>, hours::period>>;
template <class Duration>
using sys_time = time_point<system_clock, Duration>;
using sys_days = sys_time<days>;
```
</div>

加えて, `sys_days`には次の特性がある.

* `sys_days`は, `system_clock::time_point`がマイクロ秒, またはナノ秒のカウントだけであるのと同様に, `system_clock`の基点(エポック)からの日数を示す.
* `sys_days`は, 切り捨てエラーなしで暗黙的に`system_clock::time_point`に変換される.
* `system_clock::time_point`は, 切り捨てエラーが含まれるため, 暗黙的に`sys_days`に変換されない.
* `system_clock::time_point_cast` または`floor`を使用した明示的な変換によって`system_clock::time_point`から`sys_days`へ変換することができる.

内部で保持する部分カレンダー型`year`, `month`, `day` をそれぞれ`y_`, `m_`, `d_` としたとき,
`year_month_day`から`sys_days`への変換時には(すなわち上記の`operator sys_days()`の呼び出し),

* `year_month_day::ok()`が`true`の場合, `sys_days`の基点から`*this`までの日数を保持する`sys_days`を返す
* そうでない場合, `y_.ok() && m_.ok() == true` ならば `sys_days{y_/m_/last}` から `days`(`duration<int32_t, ratio_multiply<ratio<24>, hours::period>>`) の数だけ`sys_days{y_,m_,last}.day()`からオフセットされた`sys_days`を返す
* そうでない場合, 未規定である

ここで`year_month_day::ok`は,
`y_.ok() && m_.ok() == true` $\land$ `1d` $\leq$ `d_` $\leq$ `(y_/m_/last).day()` であるとき `true` を, そうでない場合, `false` を返すメンバ関数である.

```cpp
constexpr std::chrono::system_clock::time_point tp = std::chrono::sys_days{2016y/May/29d}; // Convert date to time_point
static_assert(tp.time_since_epoch() == 1'464'480'000'000'000us);
constexpr auto ymd = std::chrono::year_month_day{std::chrono::floor<days>(tp)}; // Convert time_point to date
static_assert(ymd == 2016y/May/29d);
constexpr auto tp = std::chrono::sys_days{2016y/May/29d} + 7h + 30min; // 2016-05-29 07:30 UTC
static_assert(year_month_day{sys_days{2017y/January/0}}  == 2016y/December/31);
static_assert(year_month_day{sys_days{2017y/January/31}} == 2017y/January/31);
static_assert(year_month_day{sys_days{2017y/January/32}} == 2017y/February/1);
```
上述した, `days`の他に, `weeks`, `months`, `years` が
<em onclick="obj=document.getElementById('openpartialcal').style; obj.display=(obj.display=='none')?'block':'none';">
<a style="font-style: normal; cursor:pointer;">次のように定義される(クリックで展開).</a>
</em>
<div id="openpartialcal" style="display:none;clear:both;">
```cpp
using weeks  = duration</* signed integer type of at least 22 bits */, ratio_multiply<ratio<7>, days::period>>;
using years  = duration</* signed integer type of at least 17 bits */, ratio_multiply<ratio<146097, 400>, days::period>>;
using months = duration</* signed integer type of at least 20 bits */, ratio_divide<years::period, ratio<12>>>;
```
</div>

* `days`, `weeks`, `months`, `years`型は, それぞれ少なくとも $\pm$40000 年の範囲をカバーする.
* `hours`のリテラル接尾語が`h`, `minutes`のリテラル接尾語が`min`, というように, 今までのリテラル接尾語は`duration`型へ対応していたが, 新規に追加される `y`, `d`といったリテラル接尾語は, `years`, `days`に対応するリテラル接尾語ではなく, 上述したように, `year`, `day`の部分カレンダー型に対応するリテラル接尾語である.
* 1 年を, 365.2425 日(グレゴリオ暦の平均長)と定義し, 1 月を, 30.436875 日$(\dfrac{1}{12})$ と定義するため,  システム時刻(`time_point`)を利用した算出結果と, `year_month_day` を利用した算出結果は異なる.
```cpp
constexpr auto date1 = sys_days{1997y/May/30d} - months{5}; // 1996-12-28 19:34:30
constexpr auto date2 = sys_days{1997y/December/29d} - years{1}; // 1996-12-28 18:10:48
```
現実のカレンダーの利用方法として, たとえば「2016 年の 5 月 29 日」を, 「2016 年の 5 月第 5 日曜日」ということもよくあり, これを表現することもできる.
```cpp
constexpr std::chrono::system_clock::time_point tp = std::chrono::sys_days{Sunday[5]/May/2016}; // Convert date to time_point
static_assert(tp.time_since_epoch() == 1'464'480'000'000'000us);
constexpr auto ymd = std::chrono::year_month_weekday{std::chrono::floor<days>(tp)}; // Convert time_point to date
static_assert(ymd == Sunday[5]/std::chrono::May/2016);
static_assert(2016y/May/29d == std::chrono::year_month_day{Sunday[5]/May/2016});

constexpr auto wdi = Sunday[5]; // wdi is the 5th Sunday of an as yet unspecified month
static_assert(wdi.weekday() == Sunday);
static_assert(wdi.index() == 5);
static_assert(std::is_same<decltype(Sunday), std::chrono::weekday>::value);
static_assert(std::is_same<decltype(wdi.index()), std::chrono::weekday_indexed>::value);
```
ここで, `Sunday`は`weekday`型であり, 日曜日を表現するリテラルとして定義され, 他にも`Monday`, `Tuesday`, `Wednesday`, `Thursday`, `Friday`, `Saturday`が
<em onclick="obj=document.getElementById('openweekday').style; obj.display=(obj.display=='none')?'block':'none';">
<a style="font-style: normal; cursor:pointer;">次のように定義される(クリックで展開).</a>
</em>
<div id="openweekday" style="display:none;clear:both;">
```cpp
namespace std::chrono {

inline constexpr chrono::weekday Sunday{0};
inline constexpr chrono::weekday Monday{1};
inline constexpr chrono::weekday Tuesday{2};
inline constexpr chrono::weekday Wednesday{3};
inline constexpr chrono::weekday Thursday{4};
inline constexpr chrono::weekday Friday{5};
inline constexpr chrono::weekday Saturday{6};

}
```
</div>
`weekday`型は, <i>Strong equality</i>[^1] を満たし, 加えて, 次のメンバ関数を持つ.

* デフォルトコンストラクタ, `unsigned`, `sys_days`, 後に取り上げている`local_days`型のオブジェクトを受け付けるコンストラクタ
* 曜日を前後に進める{前|後}置{イン|デ}クリメント演算子
* `weekday`, `days`型のオブジェクトを受け付けて曜日の計算を行う`+`, `-`の二項演算子
* 二項演算子と同様の計算を行い自身に代入する`+=`, `-=` の複合代入演算子
* 指定された曜日が適切であるかどうかをチェックする`ok`
* `operator []`

`operator []`は, `unsigned`型を引数として呼び出すと, `weekday_indexed`型のオブジェクトが返され, `last_spec`型のオブジェクトを引数として呼び出すと, `weekday_last`型のオブジェクトが返される.

* `weekday_indexed`型は, 月の第 1, 第 2, 第 3, 第 4 または第 5 曜日を表すために使用され, 上記の通り, `weekday`メンバ関数, `index`メンバ関数を持つ他, `ok`メンバ関数を持つ.
* `weekday_last`型は, 月の最後の`weekday`を表すために使用され, `weekday`メンバ関数, `ok`メンバ関数を持つ,
* `last_spec`型は, 最終日を表すために使用され, 同型のオブジェクト`last`が`chrono`名前空間下に定義される.

例えば次のようにして, ある月の最終日, 最終 $X$ 曜日などを表現することができる.
```cpp
auto today = std::chrono::year_month_day{std::chrono::floor<std::chrono::days>(std::chrono::system_clock::now())};
auto last_day = today.year()/today.month()/last; // last day of this month
auto last_Sunday = today.year()/today.month()/Sunday[last]; // last Sundayday of this month

static_assert(std::is_same<decltype(Sunday[5]), std::chrono::weekday_indexed>::value);
static_assert(std::is_same<decltype(Sunday[last]), std::chrono::weekday_last>::value);
```
他に, 年を未指定とし, 特定の月日を表す, `month_day`, 月の最終日を表す`month_day_last`, $N$ 番目の曜日を表す`month_weekday`, 月の最終曜日を表す`month_weekday_last`と,
日を未指定とし, 特定の年月を表す, `year_month`, 前述した`year_month_day`, 特定の年月の最終日を表す`year_month_day_last`, 特定の年月の $N$ 番目の曜日を表す`year_month_weekday`,
特定の年月の最終曜日を表す`year_month_weekday_last` が提供される.
```cpp
constexpr auto md = February/1d;
static_assert(std::is_same<decltype(md), std::chrono::month_day>::value);
constexpr auto mdl = February/last;  // mdl is the last day of February of an as yet unspecified year
static_assert(mdl.month() == February);
static_assert(std::is_same<decltype(mdl), std::chrono::month_day_last>::value);
constxpr auto mw = February/Sunday[5];
static_assert(std::is_same<decltype(mw), std::chrono::month_weekday>::value);
constexpr auto mwl = February/Sunday[last];
static_assert(std::is_same<decltype(mwl), std::chrono::month_weekday_last>::value);
constexpr auto ym = 2016y/February;
static_assert(std::is_same<decltype(ym), std::chrono::year_month>::value);
constexpr auto ymd = 2016y/February/1d;
static_assert(std::is_same<decltype(ymd), std::chrono::year_month_day>::value);
constexpr auto ymdl = 2016y/February/last;
static_assert(std::is_same<decltype(ymdl), std::chrono::year_month_day_last>::value);
constexpr auto ymw = 2016y/February/Sunday[1];
static_assert(std::is_same<decltype(ymw), std::chrono::year_month_weekday>::value);
constexpr auto ymwl = 2016y/February/last;
static_assert(std::is_same<decltype(ymwl), std::chrono::year_month_day_last>::value);
```
フルカレンダー型, 部分カレンダー型, `sys_days`型の全てで, `operator <<`のオーバーロードによる IO ストリームへの出力機能が提供される他, 非メンバ関数として, `to_stream`, `from_stream` が提供される.
これらはそれぞれ, 指定されたフォーマットの通りに出力する機能と, 指定されたフォーマットを使用して入力ストリームから解析する機能を持つ.
```cpp
std::cout << std::chrono::sys_days{Sunday[5]/May/2016} << std::endl; // 2016-05-29
std::chrono::to_stream(std::cout, "%b/%d/%Y %A %T", std::chrono::sys_days{2016y/May/29d} + 30min); // May/29/2016 Sunday 00:30:00

auto is = std::istringstream{"2016-5-26"};
auto tp = std::chrono::sys_days{};
std::chrono::from_stream(in, "%F", tp);
if (!is.fail()) std::cout << tp << std::endl; // 2016-05-26
```
また, `time_of_day`クラスが提供される. これは, `hours`, `minutes`, `seconds`, `duration<Rep, Period>` の 4 つに対する特殊化が行われており,
それぞれ午前 0 時からの時間, 時間:分, 時間:分:秒, 時間:分:秒:$X$ といった書式設定ができる.
```cpp
std::chrono::time_of_day<std::chrono::hours> todh(1h);
todh.make12();
std::cout << todh << '\n'; // 1am
todh.make24();
std::cout << todh << '\n'; // 0100

std::chrono::time_of_day<std::chrono::minutes> todm(1h + 30min);
todm.make12();
std::cout << todm << '\n'; // 1:30am
todm.make24();
std::cout << todm << '\n'; // 01:30

std::chrono::time_of_day<std::chrono::seconds> tods(1h + 30min + 30s);
tods.make12();
std::cout << tods << '\n'; // 1:30:30am
tods.make24();
std::cout << tods << '\n'; // 01:30:30

std::chdono::time_of_day<std::chrono::milliseconds> todms(1h + 30min + 30s + 30ms);
todms.make12();
std::cout << todms << '\n'; // 1:30:30.030am
todms.make24();
std::cout << todms << '\n'; // 01:30:30.030
```

### タイムゾーン
タイムゾーンライブラリは, [IANA Time Zone Database](http://www.iana.org/time-zones) のパーサーとして提供される[^2].
[IANA Time Zone Database](http://www.iana.org/time-zones) には, UTC からのオフセットと地域の省略名[^3]が含まれており, さらに該当する場合, 夏時間(サマータイム)のルールも含まれる.
これを表現した, `tzdb`, またバージョンごとの`tzdb`のリストとなっている`tzdb_list`を介して, 任意の`tzdb`にアクセスすることができる.
`tzdb_list`はシングルトンであり, 非メンバ関数`get_tzdb_list`からその参照を得て利用する.
<em onclick="obj=document.getElementById('opentimezones').style; obj.display=(obj.display=='none')?'block':'none';">
<a style="font-style: normal; cursor:pointer;">関連する宣言を下記に抜粋する(クリックで展開).</a>
</em>
<div id="opentimezones" style="display:none;clear:both;">
```cpp
namespace std { namespace chrono {

struct local_t {};
template <class Duration>
using local_time = time_point<local_t, Duration>;
using local_seconds = local_time<seconds>;
using local_days = local_time<days>;

struct sys_info {
    sys_seconds   begin;
    sys_seconds   end;
    seconds       offset;
    minutes       save;
    string        abbrev;
};

struct local_info {
    enum {unique, nonexistent, ambiguous} result;
    sys_info first;
    sys_info second;
};

enum class choose {earliest, latest};

class time_zone {
public:
    time_zone(const time_zone&) = delete;
    time_zone& operator=(const time_zone&) = delete;
    const string& name() const noexcept;
    template <class Duration> sys_info   get_info(sys_time<Duration> st)   const;
    template <class Duration> local_info get_info(local_time<Duration> tp) const;

    template <class Duration>
    sys_time<typename common_type<Duration, seconds>::type>
    to_sys(local_time<Duration> tp) const;

    template <class Duration>
    sys_time<typename common_type<Duration, seconds>::type>
    to_sys(local_time<Duration> tp, choose z) const;

    template <class Duration>
    local_time<typename common_type<Duration, seconds>::type>
    to_local(sys_time<Duration> tp) const;
};

struct tzdb {
    string            version;
    vector<time_zone> zones;
    vector<link>      links;
    vector<leap>      leaps;

    const time_zone* locate_zone(string_view tz_name) const;
    const time_zone* current_zone() const;
};

class tzdb_list {
    atomic<tzdb*> head_{nullptr};  // exposition only
public:
    class const_iterator;
    const tzdb& front() const noexcept;
    const_iterator erase_after(const_iterator p) noexcept;

    const_iterator begin() const noexcept;
    const_iterator end()   const noexcept;

    const_iterator cbegin() const noexcept;
    const_iterator cend()   const noexcept;
};

} }
```
</div>

* `local_time`は`local_t`という空の擬似クロック型が指定されており, これは当然 C++ の Clock ライブラリコンセプトを満たしていないが, 未定義のタイムゾーンに関するローカル時刻であることを示す.
* `sys_info`構造体は, `time_zone`と`sys_time`, または`local_time`の組み合わせ, および`zoned_time`から取得することができる. 実質的には, `time_zone`と`sys_time`のペアであり, 低レベル API を表現する. `sys_time`から`local_time`への通常の変換では, 暗黙的にこの構造体が使用される.
    * `begin`, `end`フィールドは, `time_zone`および`time_point`について`offset`と`abbrev`が$[$`begin`, `end`$)$ であることを示す.
    * `offset`フィールドは, 関連する`time_zone`および`time_point`に有効な UTC オフセットを示す(`offset = local_time - sys_time`).
    * `save`フィールドは, 通常`local_time`と`sys_time`の変換では必要のない"余分な"情報であるが, サマータイムの対応で必要となる. `save != 0min`の場合, この `sys_info` はサマータイムの時間帯にあると判断する. `offset - save`によって, この`time_zone`がサマータイムに対応できていない可能性を導出できる. しかし, この情報は正式なものではなく, そのような情報を確実に取得する唯一の方法は, `save == 0min`である`sys_info`を返す`time_point`と, 確認したい`time_zone`を照会することである.
    * `abbrev`フィールドは, 関連する`time_zone`および`time_point`に使用される現在の略語を示す. 略語は, `time_zone`間で一意でないため, 略語を`time_zone`と UTC のオフセットに確実にマッピングすることはできない
    * IO ストリームに対応している. `zoned_time zt = { "Asia/Tokyo", system_clock::now() }; std::cout << zt.get_info() << '\n';`
* `local_info`構造体は, 低レベル API を表す. `local_time`から`sys_time`への通常の変換では, 暗黙的にこの構造体が使用される.
    * `local_time`から`sys_time`への変換が唯一(サマータイムでない)で, `result == unique` である場合, `first`が正しい`sys_info`がセットされ, `second`が 0 で初期化される.
    * 変換が存在しない(`result == noexistent`)[^6]場合, `first`は`local_time`の直前で終了する`sys_info`がセットされ, `second`は`local_time`の直後に開始する`sys_info`がセットされる.
    * 変換が曖昧(`result == ambiguous`)[^6]な場合, `first`は`local_time`の直後に終了する`sys_info`がセットされ, `second`は`local_time`の直前で開始する`sys_info`がセットされる.
    * IO ストリームに対応している. `std::cout << get_tzdb().current_zone()->get_info(local_days{2016y/May/29d}) << '\n';`
* `time_zone`構造体は, 特定の地域の全てのタイムゾーン遷移を表現する. データベースの初期化の過程で, 現在地のタイムゾーン, およびタイムゾーンの情報をなんらかの方法[^4][^5]で構築する. <i>Strong ordering</i>[^1] を満たす.
    * `name`メンバ関数によって, `time_zone`の名前[^3]を取得できる.
    * `get_info`メンバ関数によって, `sys_info`, `local_info`を取得できる.
    * `to_sys`メンバ関数によって, `sys_time`, `local_time`を取得できる.
        * `time_zone::to_sys(local_time<Duration> tp) const;`: 少なくとも`seconds`と同じぐらいの`sys_time`であり, 引数の精度がさらに高ければそれに合わせられる. `tp`から`sys_time`への変換が曖昧[^6]である場合, `ambiguous_local_time`例外をスローする[^7]. `tp`から`sys_time`への変換が存在しない[^6]場合, `nonexistent_local_time`例外をスローする[^8].
        * `time_zone::to_sys(local_time<Duration> tp, choose z) const;`: 少なくとも`seconds`と同じぐらいの`sys_time`であり, 引数の精度がさらに高ければそれに合わせられる. `tp`から`sys_time`への変換が曖昧[^6]があいまいである場合, `z == choose::earliest`の場合は, サマータイム以前の`sys_time`を返し, `z == choose::latest`の場合は, サマータイム以後の`sys_time`を返す. `tp` が 2 つの UTC `time_point`の間に存在しない時間を表す場合, 2 つの UTC `time_point`は同じになり, UTC `time_point`が返される.
        * `time_zone::to_local(sys_time<Duration> tp) const;`: `tp` と自身の`time_zone`に関連づけられた`local_time`を返す.    
* `tzdb`は, 前述した通り, タイムゾーンデータベースを表現する.
    * `version`は, そのデータベースバージョンを表す. `zones`, `links`, `leaps`は, 検索の高速化のために, 構築時に昇順ソートされる.
    * `locate_zone`メンバ関数から, 与えられた`string_view`オブジェクトと`name()`が等価である`time_zone`が見つかった場合, その`time_zone`へのポインタを取得できる. そうでない場合, 与えられた`string_view`と`link.name()`(ここで, `link`は後述している`time_zone`の代替名を表現するクラスである)が等価である`link`が見つかった場合, `zone.name() == link.target()`の`time_zone`ポインタが取得できる. そうでない場合, `runtime_error`例外を送出する. 例外送出以外でこの関数から処理が返るとき, 返される戻り値は必ず有効な`time_zone`へのポインタである.
    * `current_zone`メンバ関数から, コンピューターに設定されたローカルタイムゾーンを取得できる.
* `tzdb_list`は, `tzdb`のアトミックポインターをもつ, `tzdb`のシングルトンリストである.　複数のバージョンのデータベースを, 同リストを介して一度に使用することができる. 例:`for (auto&& v : get_tzdb_list()) { std::cout << v << '\n'; }`
    * `front`メンバ関数によって, 先頭`tzdb`の参照を得ることができる. これは, `reload_tzdb`非メンバ関数に対してスレッドセーフである.
    * `erase_after`メンバ関数によって, 与えられたイテレータの後に参照する`tzdb`を消去する. 消去された要素の次の要素を指すイテレータが返される. そのような要素が存在しない場合, メンバ関数`end`を呼び出し, その結果を返す. なお, ここで消去された`tzdb`を参照することを除いて, ポインター, 参照, イテレータは無効にならない. また, メンバ関数`begin`を呼び出し, それによって参照される`tzdb`を消去することはできない.
    * `begin`メンバ関数によって, コンテナ内の最初の`tzdb`を参照するイテレータ取得できる. `cbegin`メンバ関数は`begin`メンバ関数の`const`版である.
    * `end`メンバ関数によって, コンテナ内の最後の`tzdb`より 1 つ後ろの位置を参照するイテレータを取得できる. `cend`メンバ関数は`end`メンバ関数の`const`版である.
    * `get_tzdb_list`非メンバ関数によって, 同リストの参照を得ることができる. 同メンバ関数への呼び出しがデータベースへの最初のアクセスである場合, データベースを初期化する. この呼び出しによってデータベースが初期化された場合, `tzdb`を一つ持つ`tzdb_list`が構築される. 同メンバ関数を一度に複数のスレッドから呼び出しても競合せず, スレッドセーフである. 何らかの理由で有効なリストの参照を返せず, 1 つ以上の有効な`tzdb`を含む場合, `runtime_error`例外を送出する.
    * `get_tzdb`非メンバ関数によって, 同リストの先頭`tzdb`の参照を得ることができる(`get_tzdb_list().front()`).
    * `locate_zone`非メンバ関数によって, 次の値を得ることができる. なお, これがデータベースへの最初のアクセスである場合, データベースを初期化する. `get_tzdb().locate_zone(tz_name);`
    * `current_zone`非メンバ関数によって, 次の値を得ることができる. `get_tzdb().current_zone();`

<i>ローカル</i>タイムゾーンデータベースは、アプリケーションがデータベースに最初にアクセスするとき, たとえば`current_zone()`を介して実装によって提供される.
アプリケーションが実行されている間, 実装はタイムゾーンデータベースの更新を選択することがある.
このアップデートは, アプリケーションによって次に挙げる関数を呼び出さない限り, アプリケーションに影響を与えることはない.
この潜在的に更新されたタイムゾーンデータベースは, <i>リモート</i>タイムゾーンデータベースと呼ぶ.
<em onclick="obj=document.getElementById('openremotetimezone').style; obj.display=(obj.display=='none')?'block':'none';">
<a style="font-style: normal; cursor:pointer;">次のように定義される(クリックで展開).</a>
</em>
<div id="openremotetimezone" style="display:none;clear:both;">
```cpp
namespace std { namespace chrono {

const tzdb& reload_tzdb();
string remote_version();

} }
```
</div>

* `reload_tzdb`非メンバ関数は, 最初にリモートタイムゾーンデータベースのチェックを行い, ローカルデータベースとリモートデータベースのバージョンが同じである場合はなにもしない. それ以外の場合, リモートデータベースは, `get_tzdb_list`非メンバ関数によってアクセスされる`tzdb_list`の先頭にプッシュされる. いずれの場合も, `get_tzdb_list().front()` が返される. この関数は, `get_tzdb_list().front()`と`get_tzdb_list().erase_after()`に対してスレッドセーフである. 何らかの理由で有効な`tzdb`の参照が戻されない場合, `runtime_error`例外が送出される.
* `remote_version`非メンバ関数は, 最新のリモートデータベースバージョンの文字列(`std::string`)を返す. リモートバージョンが利用できない場合, 空の文字列が返される. 空でない場合, これを`get_tzdb_list().version`と比較して, ローカルデータベースとリモートデータベースが同等かどうかをチェックできる.

`zoned_traits`, `zoned_time`を利用することで, `sys_days`, `local_days`といった`time_point`を`tzdb`データベースと関連付けることができる.
<em onclick="obj=document.getElementById('openzoned_traits').style; obj.display=(obj.display=='none')?'block':'none';">
<a style="font-style: normal; cursor:pointer;">次のように定義される(クリックで展開).</a>
</em>
<div id="openzoned_traits" style="display:none;clear:both;">
```cpp
namespace std { namespace chrono {

template <class T> struct zoned_traits {};
template <>
struct zoned_traits<const time_zone*> {
    static const time_zone* default_zone();
    static const time_zone* locate_zone(string_view name);
};

} }
```
</div>

`zoned_traits`によって, `zoned_time`のデフォルトコンストラクタの動作をカスタマイズすることができる.

* `zoned_traits<const time_zone*>::default_zone();` は, `std::chrono::locate_zone("UTC")` を返す.
* `zoned_traits<const time_zone*>::locate_zone(string_view name);` は, `std::chrono::locate_zone(name)` を返す.

`zoned_time`は, `Duration`の精度で, `time_zone`と`time_point`の論理区切りを表す. <i>Strong equality</i>[^1]を満たす.
<em onclick="obj=document.getElementById('openzoned_time').style; obj.display=(obj.display=='none')?'block':'none';">
<a style="font-style: normal; cursor:pointer;">次のように定義される(クリックで展開).</a>
</em>
<div id="openzoned_time" style="display:none;clear:both;">
```cpp
template <class Duration, class TimeZonePtr = const time_zone*>
class zoned_time {
public:
    using duration = common_type_t<Duration, seconds>;

private:
    TimeZonePtr        zone_;  // exposition only
    sys_time<duration> tp_;    // exposition only

public:
    zoned_time();
    zoned_time(const zoned_time&) = default;
    zoned_time& operator=(const zoned_time&) = default;

             zoned_time(const sys_time<Duration>& st);
    explicit zoned_time(TimeZonePtr z);
    explicit zoned_time(string_view name);

    template <class Duration2>
        zoned_time(const zoned_time<Duration2>& zt) noexcept;

    zoned_time(TimeZonePtr z,    const sys_time<Duration>& st);
    zoned_time(string_view name, const sys_time<Duration>& st);

    zoned_time(TimeZonePtr z,    const local_time<Duration>& tp);
    zoned_time(string_view name, const local_time<Duration>& tp);
    zoned_time(TimeZonePtr z,    const local_time<Duration>& tp, choose c);
    zoned_time(string_view name, const local_time<Duration>& tp, choose c);

    template <class Duration2, class TimeZonePtr2>
        zoned_time(TimeZonePtr z, const zoned_time<Duration2, TimeZonePtr2>& zt);
    template <class Duration2, class TimeZonePtr2>
        zoned_time(TimeZonePtr z, const zoned_time<Duration2, TimeZonePtr2>& zt, choose);

    zoned_time(string_view name, const zoned_time<Duration>& zt);
    zoned_time(string_view name, const zoned_time<Duration>& zt, choose);

    zoned_time& operator=(const sys_time<Duration>& st);
    zoned_time& operator=(const local_time<Duration>& ut);

             operator sys_time<duration>()   const;
    explicit operator local_time<duration>() const;

    TimeZonePtr          get_time_zone()  const;
    local_time<duration> get_local_time() const;
    sys_time<duration>   get_sys_time()   const;
    sys_info             get_info()       const;
};
```
</div>

* invariant な`zoned_time<Duration>`は常に有効な`time_zone`を参照し, 曖昧でない時間を表す.
* デフォルコンストラクタは, `zone_`を`zoned_traits::default_zone()`で初期化し, `tp_`をデフォルト構築して`zoned_time`を構築する.
* コピーコンストラクタは, 関連する`time_zone`を転送する. `Duration`が`noexcept`コピーコンストラクタである場合, `zoned_time<Duration>`は`noexcept`コピーコンストラクタである.
* `zoned_time(const sys_time<Duration>& st)`: `zone_`を`zoned_traits::default_zone`で初期化し, `tp_`を`st`で初期化して`zoned_time`を構築する.
* `zoned_time(TimeZonePtr z)`: `std::move(z)`で`zone_`を初期化し, `zoned_time`を構築する. このとき, `z`は有効な`time_zone`を指していなければならない.
* `zoned_time(string_view name)`: `zoned_traits::locate_zone(name)`で`zone_`を初期化し, `tp_`をデフォルト構築して`zoned_time`を構築する.
* `zoned_time(const zoned_time<Duration2, TimeZonePtr>& y) noexcept`: `x == y`となる`zoned_time`, `x`を構築する.
* `zoned_time(TimeZonePtr z, const sys_time<Duration>& st)`: `zone_`を`std::move(z)`で初期化し, `tp_`を`st`で初期化して`zoned_time`を構築する. このとき, `z`は有効な`time_zone`を指していなければならない.
* `zoned_time(string_view name, const sys_time<Duration>& st)`: `{zoned_traits<TimeZonePtr>::locate_zone(name), st}`と同等の構築を行う.
* `zoned_time(TimeZonePtr z, const local_time<Duration>& tp)`: `zone_`を`std::move(z)`で初期化し, `tp_`を`zone_->to_sys(t)`で初期化して`zoned_time`を構築する. このとき, `z`は有効な`time_zone`を指していなければならない.
* `zoned_time(string_view name, const local_time<Duration>& tp)`: `{zoned_traits<TimeZonePtr>::locate_zone(name), tp}`と同等の構築を行う.
* `zoned_time(TimeZonePtr z, const local_time<Duration>& tp, choose c)`:  `zoned_`を`std::moev(z)`で初期化し, `tp`を`zone_->to_sys(t, c)`で初期化して`zoned_time`を構築する. このとき, `z`は有効な`time_zone`を指していなければならない.
* `zoned_time(string_view name, const local_time<Duration>& tp, choose c)`: `{zoned_traits<TimeZonePtr>::locate_zone(name), tp, c}`と同等の構築を行う.
* `zoned_time(TimeZonePtr z, const zoned_time<Duration2, TimeZonePtr2>& y)`: `zone_`を`std::move(z)`で初期化し, `tp_`を`z.tp_`で初期化して`zoned_time`を構築する. このとき, `z`は有効な`time_zone`を指していなければならない.
* `zoned_time(TimeZonePtr z, const zoned_time<Duration2, TimeZonePtr2>& y, choose)`: `{z, y}`と同等の構築を行う. このとき, `z`は有効な`time_zone`を指していなければならない. `choose`パラメータを渡すことができるが, これによって挙動が変わることはない.
* `zoned_time(string_view name, const zoned_time<Duration>& y)`: `{zoned_traits<TimeZonePtr>::locate_zone(name), y}`と同等の構築を行う.
* `zoned_time(string_view name, const zoned_time<Duration>& y, choose c)`: `{locate_zone(name), y, c}`と同等の構築を行う. `choose`パラメータを渡すことができるが, これによって挙動が変わることはない.
* `operator=(const local_time<Duration>& lt)`: 代入後, `get_local_time() == lt`となるよう代入し`*this`を返す. この代入は, `get_time_zone`の戻り値には影響しない.
* `operator sys_time<duration>() const`: `get_sys_time()`を返す.
* `operator local_time<duration>() const`: `get_local_time()` を返す.
* `get_time_zone`メンバ関数によって, `zone_`のポインタを取得できる.
* `get_local_time`メンバ関数によって, 構築時に設定されたタイムゾーンでの`local_time`オブジェクトを取得できる(`retunr zone_->to_local(tp_);`).
* `get_sys_time`メンバ関数によって, 構築時に設定されたタイムゾーンでの`sys_time`オブジェクトを取得できる(`return tp_;`).
* `get_info`メンバ関数によって, 構築時に設定されたタイムゾーンでの`sys_info`オブジェクトを取得できる(`return zone_->get_info(tp_);`).

同ライブラリを利用して, 例えば次のように, ある日時の東京の時間帯を得ることができる[^5].
```cpp
auto tp1 = std::chrono::sys_days{2016y/May/29d} + 7h + 30min + 6s + 153ms; // 2016-05-29 07:30:06.153 UTC
std::chrono::zoned_time zt1 = {"Asia/Tokyo", tp1};
std::cout << zt1 << '\n'; // 2016-05-29 16:30:06.153 JST

auto tp2 = std::chrono::local_days{2016y/May/29d} + 7h + 30min + 6s + 153ms; // 2016-05-29 07:30:06.153 JTC
auto zt2 = std::chrono::zoned_time{"Asia/Tokyo", tp2};
std::cout << zt << '\n'; // 2016-05-29 07:30:06.153 JST
```

`leap`は, タイムゾーンデータベースの初期化時に構築され, タイムゾーンデータベースに格納されるクラスであり, 主に閏秒を扱うクラスである. 同クラスは, <i>Strong ordering</i>[^1]を満たす.
<em onclick="obj=document.getElementById('openclassleap').style; obj.display=(obj.display=='none')?'block':'none';">
<a style="font-style: normal; cursor:pointer;">次のように定義される(クリックで展開).</a>
</em>
<div id="openclassleap" style="display:none;clear:both;">
```cpp
class leap
{
    sys_seconds date_;  // exposition only
public:
    leap(const leap&)            = default;
    leap& operator=(const leap&) = default;

    // Undocumented constructors
    sys_seconds date() const;
};
```
</div>

* `date`メンバ関数によって, `date_`を取得できる. `date_`には閏秒挿入の日付が格納されている.
* 閏秒挿入の全ての日付を`for (auto& l : get_tzdb().leaps) std::cout << l.date() << '\n';`で確認できる.

またタイムゾーンデータベースの構築時に作成される, `time_zone`の代替名を表現する`link`というクラスも提供される.


### Clock

新たに `utc_clock`, `tai_clock`, `gps_clock`, `file_clock` の 4 つのクロック, 
また, その`time_point`型のエイリアス(`utc_time`, `utc_seconds`, `tai_time`, `tai_seconds`, `gps_time`, `gps_seconds`) が提供される.

* `utc_clock` は, 協定世界時(UTC)を表現するクロックであり, 1970 年 1 月 1 日木曜日午後 00:00:00 分からの時間を測定する. これには, 閏秒が含まれる.
* `tai_clock`は, 国際原始時計(TAI)を表現するクロックであり, 1958 年 1 月 1 日 00:00:00 からの時間を測定し, この日の UTC(1957-12-31 23:59:50 UTC)よりも 10 秒前にオフセットされている. これには, 閏秒が含まれない[^9].
* `gps_clock`は GPS 時刻を表現するクロックであり, UTC 1980 年 1 月 6 日 00:00:00 からの時間を測定する. 閏秒は含まれない[^10].
* `file_clock`は, C++20 で追加されたエイリアス, `using file_time_type = std::chrono::time_point<std::chrono::file_clock>;`で利用されるファイルクロックである[^11].

### 試用
同ライブラリを利用した任意月のカレンダーを出力するプログラムは, [既にあった](http://d.hatena.ne.jp/yohhoy/20180322/p1)のだが, 特別何か別のものは思いつかないので,
とりあえず, 任意年の全ての月のカレンダーを出力するプログラムを書いて試用.
```cpp
#include <algorithm>
#include <array>
#include <chrono>
#include <iostream>
#include <iomanip>
#include <utility>

namespace ns {

template <class> struct weeks_init;
template <std::size_t... s>
struct weeks_init<std::index_sequence<s...>> {
    constexpr weeks_init() = default;
    constexpr std::array<std::chrono::weekday, sizeof...(s)> operator()() const noexcept { return {{ std::chrono::weekday{s}... }}; }
};
constexpr auto weeks = weeks_init<std::make_index_sequence<7>>()();

inline void print_weeks(std::ostream& os)
{
    namespace sc = std::chrono;
    std::copy(std::begin(weeks), std::end(weeks), std::ostream_iterator<sc::weekday>(os, "  "));
    os << '\n';
}

} // namespace ns

struct print_calendar_year {
    explicit constexpr print_calendar_year(std::chrono::year y) noexcept
        : y_(y) {}

    friend std::ostream& operator<<(std::ostream& os, const print_calendar_year& this_)
    {
        using namespace std::chrono_literals;
        namespace sc = std::chrono;
        constexpr int width = 5;

        for (unsigned i = 1u, uweek = static_cast<unsigned>(sc::weekday{sc::sys_days{this_.y_/sc::January/1d}}); i <= 12u; ++i) {
            auto lastday = (this_.y_/sc::month{i}/sc::last).day();
            os << std::setw(ns::weeks.size() * width / 2) << sc::month{i} << '\n';
            ns::print_weeks(os << std::setw(width));

            unsigned k = 0;
            for (; k < uweek; ++k) os << std::setw(width) << " ";
            for (sc::day d{1}; d <= lastday; ++d) {
                os << std::setw(width) << static_cast<unsigned>(d);
                if (++k > 6) {
                    k = 0;
                    os << '\n';
                }
            }
            if (k) os << '\n';
            uweek = k;
        }
        return os;
    }
private:
    std::chrono::year y_;
};

int main()
{
    std::cout << print_calendar_year{std::chrono::year{2000}} << std::endl;
}
```
[実行結果](https://wandbox.org/permlink/qdkXXRJFNGPTdDg2). <br>

タイムゾーンに関するサンプルは, [元の実装のドキュメント](https://howardhinnant.github.io/date/tz.html)で多く取り上げられている. フライトタイムの計算や, IANA タイムゾーンデータベースを利用しないカスタムタイムゾーンを作成する例などが掲示されている.

### 感想

* とてもよく作り込まれていて, 使いやすそうに感じる. 
* C++ にこのような高レベル API が導入されるのは, 少し新鮮.

[^1]: この一つ前の ISO C++ 委員会による国際会議で C++20 に追加された [P0515 Consistent comparison](http://open-std.org/JTC1/SC22/WG21/docs/papers/2017/p0515r2.pdf) で挙げられている comparison category types での呼称を用いている. 参照: [Consistent/three-way comparison](https://roki.hateblo.jp/entry/2017/11/27/Consistent/three-way_comparison)
[^2]: タイムゾーンライブラリの型とその関係性を示した[図](https://howardhinnant.github.io/date/tz_types.jpeg)を, 作者のドキュメントページから見ることができる.
[^3]: [List of tz database time zones](https://en.wikipedia.org/wiki/List_of_tz_database_time_zones)
[^4]: [元の実装](https://github.com/HowardHinnant/date)を見ると, 現在地のタイムゾーン取得においては, [Linux および Mac では特定ファイル(/usr/share/zoneinfo, /usr/share/zoneinfo/uclibc) を読み込み](https://github.com/HowardHinnant/date/blob/38c5ca38bb73b292b72e088c31595add564d31f6/src/tz.cpp#L321-L372), [Windows ではレジストリ値を読み込んでいる](https://github.com/HowardHinnant/date/blob/38c5ca38bb73b292b72e088c31595add564d31f6/src/tz.cpp#L3615-L3631). レジストリ値から取得されたネイティブな現在のタイムゾーン名が標準のものと一致する保証はなく, 特に Windows の場合, 得られる名前は必ず標準と異なるものであるため, 標準の名前と関連づけるマッピングが行われる. 元の実装では, Windows の場合ではタイムゾーンデータベースの取得の際に, [xml ファイル](http://unicode.org/repos/cldr/trunk/common/supplemental/windowsZones.xml) を[取得している](https://github.com/HowardHinnant/date/blob/38c5ca38bb73b292b72e088c31595add564d31f6/src/tz.cpp#L3263-L3271).
[^5]: [元の実装](https://github.com/HowardHinnant/date)を見ると, OS のタイムゾーンデータベースを利用せず, リモート API があるときは [OS 依存またはサードパーティ製のライブラリを利用してデータを取得](https://github.com/HowardHinnant/date/blob/38c5ca38bb73b292b72e088c31595add564d31f6/src/tz.cpp#L3243-L3273)し, そうでないとき OS のタイムゾーンデータベースを利用する. [元の実装](https://github.com/HowardHinnant/date)で OS のタイムゾーンデータベースを試用する際には, `DUSE_OS_TZDB=1`をセットしてビルドする. Windows 環境がないので筆者にはわからないが, [Windows では OS のタイムゾーンデータベースを利用できないようだ](https://github.com/HowardHinnant/date/blob/e7e1482087f58913b80a20b04d5c58d9d6d90155/CMakeLists.txt#L51).
[^6]: サマータイムの開始と終了で, 存在しないローカル時間(`nonexistent_local_time`)と重複するローカル時間(`ambiguous_local_time`)という概念が生じる.
[^7]: 2016-11-06 01:30:00 EDT は, 2016-11-06 05:30:00 UTC と 2016-11-06 06:30:00 UTC
 になりうる. `try { auto zt = zoned_time{"America/New_York", local_days{Sunday[1]/November/2016} + 1h + 30min}; } catch (const ambiguous_local_time&) { }`
[^8]: 2016-03-13 02:30:00 EDT は, 2016-03-13 02:00:00 EST と 2016-03-13 03:00:00 EDT の間にあるため存在しない. どちらも, 2016-03-13 07:00:00 UTC と等価である. `try { auto zt = zoned_time{"America/New_York", local_days{Sunday[2]/March/2016} + 2h + 30min}; } catch (const noexistent_local_time&) {}`
[^9]: 閏秒が UTC に挿入される度に, UTC は TAI の 1 秒遅れとなる. 1961 年発祥の旧 UTC, 1972 年の特別調整, 1972 年から 2017 年 1 月まで行われた27 回の閏秒調整の過程を踏み, UTC は現在 TAI に対して 37 秒遅れている. 参考: [http://jjy.nict.go.jp/mission/page1.html](http://jjy.nict.go.jp/mission/page1.html)
[^10]: `tai_clock`同様, UTC に閏秒が挿入されるたびに UTC の 1 秒後を表現することとなる. 2017 年時点で, UTC は GPS の 18 秒前 にある. 余談: [GPS が UTC との差分を計算する方法に関して](https://nyanchew.com/jp/gps%E3%81%AF%E3%81%A9%E3%81%AE%E3%82%88%E3%81%86%E3%81%AB%E3%81%97%E3%81%A6%E3%81%86%E3%82%8B%E3%81%86%E7%A7%92%E3%82%92%E6%8C%BF%E5%85%A5%E3%81%99%E3%82%8B%E3%81%8B).
[^11]: `file_time_type`は C++17 時点で既にエイリアスとして追加されているが, `using file_time_type = std::chrono::time_point</*trivial-clock*/>;`と記されており, 具体的なクロック型は明記されていなかった.
[^12]: [yohhoy](https://twitter.com/yohhoy/status/985830560013500416) さんに`last`の属する名前空間に関する追加情報を頂いた. ありがとうございます.
