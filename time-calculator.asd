(defsystem "time-calculator"
    :serial t
    :depends-on (:cl-ppcre)
    :components ((:file "package")
                 (:file "time-calculator")
                 (:file "report")) )
