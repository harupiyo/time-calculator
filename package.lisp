(in-package :cl-user)

(defpackage time-calculator
  (:use :common-lisp :cl-ppcre)
  (:nicknames tc)
  (:export :diff-time :min-to-hour :*unit-price* :report :ceiling-by-unit))
