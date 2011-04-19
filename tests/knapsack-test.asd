;;; -*- lisp -*-

(asdf:defsystem knapsack-test
  :depends-on (:knapsack)
  :components
    ((:file "lisp-unit")
     (:file "package")
     (:file "unit-tests"))
  :serial t)
