
(cl:defpackage :knapsack-test
  (:nicknames :rs-test)
  (:use :common-lisp :knapsack :lisp-unit)
  (:export #:run-tests))

(cl:defpackage :knapsack-test-schema-update
  (:nicknames :rs-tsu)
  (:use :common-lisp :knapsack))

