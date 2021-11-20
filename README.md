# time-calculator

区切られた 開始時間-終了時間 を積算する目的で作った簡易的な時間の計算ライブラリー、及び時給計算の明細を作成するアプリケーションです。

# 利用方法

利用方法及び概要を示します。

```
CL-USER> (ql:quickload :time-calculator)
CL-USER> (in-package :tc)
TC> (min-to-hour
      (+ (diff-time "16:46" "18:24")
    	 (diff-time "11:32" "14:46")) ; 292 "min"
    	 30 ; 時間の差分だけでなしに、かかった時間を直接指定可能
      )
5.366
"hour"

TC> (setq *unit-price* 3000) ; 時給を設定
TC> (report "@harupiyo Common Lisp でtime-calclulator 作り"
      "14:18-15:46"
      "16:33-17:28"        
      "21:09-21:44"        
      "21:51-21:55"        
      322
      "22:03-22:18"        
      "11:10-11:14"        
      "11:57-12:02"        
      "12:09-12:20"        
      "09:49-10:00"
      )
【ご請求】￥27,750 (9時間15分) @harupiyo  Common Lisp でtime-calclulator 作り
-----
- 14:18-15:46 ... 88分
- 16:33-17:28 ... 55分
- 21:09-21:44 ... 35分
- 21:51-21:55 ...  4分
- 322分
- 22:03-22:18 ... 15分
- 11:10-11:14 ...  4分
- 11:57-12:02 ...  5分
- 12:09-12:20 ... 11分
- 09:49-10:00 ... 11分
-----
total: 550分 ==(15分単位)==> 555分 / 9時間15分(*1)
ご利用料金: (*1) x ￥3,000 = ￥27,750
NIL
```

# Install 

ql:quickload できる場所にソースコードを設置します。

```
$ mkdir ~/common-lisp/
$ git clone https://github.com/harupiyo/time-calculator ~/common-lisp/time-calculator
```

あとは [利用方法](#利用方法) のように使います。

# API

利用方法にある例で使われている関数と、利用方法には出ていない関数をまとめて説明します。

## diff-time

２つの時間を指定して、その差を分で返します。

```
(diff-time "16:46" "18:24") => 98 "min"
(diff-time "18:24" "16:46") => 98 "min" ; ２つの時間は、どちらが先でもかまいません
(diff-time "23:50" "25:10") => 80 "min" ; 日をまたいだ計算をしたい時には24:00 以降の時間として指定してください
```

## min-to-hour

分を指定して、時間に換算します。

```
(min-to-hour 292) => 4.866667 "hour"
```

- ここで出てくる時間は10進数であり、時給計算する際にそのまま時給に掛ければよいので便利です。
- 人間の時間の感覚では直感的ではありません。1.5時間と言われて1時間30分だと認識するには頭の切り替えが必要です。その場合には次の hour-by-decimal->hour-min 関数を使ってください。

## hour-by-decimal->hour-min

10進数での時間表現(例えば min-to-hour の返り値がそうです) を与えると、多値で時間と分を返します。

````
(hour-by-decimal->hour-min 1.25) => 1 15 ; 1.25時間は1時間15分であることを示しています。
```

## ceiling-by-unit

defun: ceiling-by-unit (min &optional (unit 15))

指定した単位 (デフォルトは15) 区切りの数字に底上げし、15 分区切りになおします。

```
(ceiling-by-unit 1)  => 15
(ceiling-by-unit 15) => 15
(ceiling-by-unit 16) => 30
```

## report

時給と複数の時間間隔を与えて、明細書を作成するアプリケーションです。

(この関数は複雑なformat 書式を使っており、カスタマイズするのはしんどいです)

```
TC> (setq *unit-price* 3000) ; 時給を設定
TC> (report "@harupiyo Common Lisp でtime-calclulator 作り"
      "14:18-15:46"
      "16:33-17:28"        
      "21:09-21:44"        
      "21:51-21:55"        
      322
      "22:03-22:18"        
      "11:10-11:14"        
      "11:57-12:02"        
      "12:09-12:20"        
      "09:49-10:00"
      )
【ご請求】￥27,750 (9時間15分) @harupiyo  Common Lisp でtime-calclulator 作り
-----
- 14:18-15:46 ... 88分
- 16:33-17:28 ... 55分
- 21:09-21:44 ... 35分
- 21:51-21:55 ...  4分
- 322分
- 22:03-22:18 ... 15分
- 11:10-11:14 ...  4分
- 11:57-12:02 ...  5分
- 12:09-12:20 ... 11分
- 09:49-10:00 ... 11分
-----
total: 550分 ==(15分単位)==> 555分 / 9時間15分(*1)
ご利用料金: (*1) x ￥3,000 = ￥27,750
NIL
```
