[日本語ドキュメントはこちらです。](/README_ja.md)

# time-calculator - Simple calculator to calculate time differences

There are two ways to use it.

1. Simple calculator to calculate start to end times. (time-calculator.lisp)
	```
	TC> (diff-time "16:46" "18:24") ; 98 "min"
	TC> (min-to-hour 98)			; 1.6333333 "hour"
	TC> (hour-by-decimal->hour-min 1.6333333) => 1 "hour" + 38 "min"
	TC> (min-to-hour
		  (+ (diff-time "16:46" "18:24")	;  98 "min"
			 (diff-time "11:32" "14:46")	; 194 "min"
			 30))							;  98 + 194 + 30 = 322 = 5.366667 "hour"
	=> 5.366667
	"hour"
	```
2. Create hourly wage statement. (report.lisp)
	```
	TC> (setq *unit-price* 3000) ; Set hourly rate.
	TC> (report "@harupiyo Make time-calculator in Common Lisp"
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
	【Billing】￥27,750 (9hrs 15min) @harupiyo Making a time-calculator in Common Lisp.
	-----
	- 14:18-15:46 ... 88min
	- 16:33-17:28 ... 55min
	- 21:09-21:44 ... 35min
	- 21:51-21:55 ...  4min
	- 322min
	- 22:03-22:18 ... 15min
	- 11:10-11:14 ...  4min
	- 11:57-12:02 ...  5min
	- 12:09-12:20 ... 11min
	- 09:49-10:00 ... 11min
	-----
	total: 550min ==(in increments of 15 minutes)==> 555min => 9 hours 15 minutes (*1)
	Charge: (*1) x ￥3,000 = ￥27,750
	NIL
	```

# Install

Git clone under `~/common-lisp/` or `~/.local/share/common-lisp/source/`, which is the standard folder of ASDF.

```
$ git clone https://github.com/harupiyo/time-calculator.git ~/common-lisp/time-calculator
```

Once the REPL is up and running, use Quicklisp to load the system.

```
CL-USER> (ql:quickload :time-calculator)
CL-USER> (in-package :tc)		; tc is the nickname for time-calculator
TC>
```

API for time-calculator is available from REPL.

# Features (API)

## diff-time

Returns the difference between two times in minutes.

```
(diff-time "16:46" "18:24") => 98 "min"
(diff-time "18:24" "16:46") => 98 "min" ; either of the two times can be first
(diff-time "23:50" "25:10") => 80 "min" ; If you want to calculate across days, specify a time after 24:00
```

## min-to-hour

Specify minutes to be converted to hours.

```
(min-to-hour 292) => 4.866667 "hour"
```

- The hours here are in decimal for convenience in hourly calculations.
- The following "hour-by-decimal->hour-min" function can be used to break it down into "hours + minutes".

## hour-by-decimal->hour-min

Get hours and minutes from the hour representation in decimal.

````
(hour-by-decimal->hour-min 1.25) => 1 15 ; 1.25h = 1h + 15min
```

## ceiling-by-unit

Ceil up to the specified number of units (default is 15), and re-decimalize to 15-minute increments.

```
(ceiling-by-unit 1  15) => 15
(ceiling-by-unit 15 15) => 15
(ceiling-by-unit 16 15) => 30
```

## report

This application is designed to generate statements by giving multiple time intervals.
Since the report format you want to output varies from time to time, please consider this as a demo version and customize it by referring to the usage examples below.

```
TC> (setq *unit-price* 3000) ; set hourly rate
TC> (report "@harupiyo Make time-calculator in Common Lisp"
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
【Billing】￥27,750 (9hrs 15min) @harupiyo Making a time-calculator in Common Lisp.
-----
- 14:18-15:46 ... 88min
- 16:33-17:28 ... 55min
- 21:09-21:44 ... 35min
- 21:51-21:55 ...  4min
- 322min
- 22:03-22:18 ... 15min
- 11:10-11:14 ...  4min
- 11:57-12:02 ...  5min
- 12:09-12:20 ... 11min
- 09:49-10:00 ... 11min
-----
total: 550min ==(in increments of 15 minutes)==> 555min => 9 hours 15 minutes (*1)
Charge: (*1) x ￥3,000 = ￥27,750
NIL
```

## calc-time-span-to-min

Calculates minutes from time expressions.
このユーティリティは、時間計測の時に便利だと思う２通りの表現をサポートします。

1. direct representation of minutes
	```
	(calc-time-span-to-min 234) => 234 "min"
	```
2. two times representing elapsed time (like diff-time)
	```
	(calc-time-span-to-min "12:34-22:33") => 599 "min"
	```

# [TODO] time-or-num About the reader macro

I tried to find a way to express time in literals like 12:34, because it is tedious to express time in quotes as "12:34" every time.
(Currently, this is not a smart solution. I will note this as a TODO to be worked on in the future.)

The reader macro is used to bring in a unique literal notation in the REPL.
The reader macro mechanism requires a leading prefix "'#' and a single character".
Thus, "12:34" would have been 12:34, but the prefixed expression "#t12:34" was used.

Here is an example of the use of the time-or-num reader macro.
```
TC> (diff-time #t10:30 #t10:00)
	=> 30
	"min".
```
[TODO] I want to do the following.
```
TC> (diff-time 10:30 10:00)
	=> 30
	"min".
```
