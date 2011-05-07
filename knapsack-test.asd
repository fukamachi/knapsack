(in-package :cl-user)
(defpackage knapsack-test-asd
  (:use :cl :asdf))
(in-package :knapsack-test-asd)

(defsystem knapsack-test
  :depends-on (:knapsack)
  :components ((:module "test"
                :serial t
                :components ((:file "lisp-unit")
                             (:file "package")
                             (:file "unit-tests")))))
