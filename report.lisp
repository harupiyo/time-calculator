(in-package :time-calculator)

;;; reporting facility

(defvar *unit-price* 3000 "時給")

(defun report (title &rest time-spans)
  (let* ((min (loop :for span :in time-spans
                    :sum (calc-time-span-to-min span)))
         (detail (report-times time-spans))
         (min-normalized (ceiling-by-unit min))
         (hour (min-to-hour min))
         (hour-normalized (min-to-hour min-normalized))
         (cost (floor (* hour-normalized *unit-price*))) 
         )
    (multiple-value-bind (h m) (hour-by-decimal->hour-min hour-normalized)
        (format t "【ご請求】￥~:D (~A時間~:[~A分~;~*~]) ~A~%-----~%~{- ~A~%~}-----~%total: ~A分 ==(15分単位)==> ~A分 / ~A時間~:[~A分~;~*~](*1)~%ご利用料金: (*1) x ￥~:D = ￥~:D~%" cost h (zerop m) m title detail min min-normalized h (zerop m) m *UNIT-PRICE* cost)
      )))

(defun report-times (time-spans)
  (loop :for span :in time-spans
        :if (integerp span)
            :collect (format nil "~A分" span)
        :else
            :collect (format nil "~A ...~3D分" span (calc-time-span-to-min span))))

(defun hour-by-decimal->hour-min (hour-by-decimal)
  "
  Usage:
    (hour-by-decimal->hour-min 1.25) => 1 15 ; 1.25時間は1時間15分
  "
  (multiple-value-bind (hour below-the-decimal-point) (floor hour-by-decimal)
    (values hour
            (floor (/ (* 60 below-the-decimal-point) 1.0)))))

(defun ceiling-by-unit (min &optional (unit 15))
  "
  指定した単位 (デフォルトは15) 区切りの数字に底上げする
  (loop :for i :from 1 :to 16
        :collect (list i \"->\"(ceiling-by-unit i)))
  =>((1 \"->\" 15) (2 \"->\" 15) (3 \"->\" 15) (4 \"->\" 15) (5 \"->\" 15) (6 \"->\" 15)
     (7 \"->\" 15) (8 \"->\" 15) (9 \"->\" 15) (10 \"->\" 15) (11 \"->\" 15) (12 \"->\" 15)
     (13 \"->\" 15) (14 \"->\" 15) (15 \"->\" 15) (16 \"->\" 30))
  "
  (multiple-value-bind (division reminder) (floor min unit)
    (* (+ division
          (if (zerop reminder) 0 1))
       unit)))

(defun calc-time-span-to-min (span)
  "
  Usage:
  (calc-time-span-to-min 1234 => 1234 \"min\"
  (calc-time-span-to-min \"12:34-22:33\") => 599 \"min\"
  (calc-time-span-to-min \"12:34 22:33\") => 599 \"min\"
  "
  (if (typep span 'integer)
      (values span "min")
      (apply #'diff-time (ppcre:all-matches-as-strings "\\d\\d:\\d\\d" span))))

