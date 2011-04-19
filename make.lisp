#|
  This file is a part of Knapsack package.
  URL: http://github.com/fukamachi/knapsack
  Copyright (c) 2006  Arthur Lemmens
  Copyright (c) 2011  Eitarow Fukamachi <e.arrows@gmail.com>

  For the full copyright and license information, please see the LICENSE.
|#

(in-package :cl-user)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *knapsack-directory* *load-pathname*))

(defun make (&key (debug t))
  (when debug
    (proclaim '(optimize (debug 3) (speed 0) (space 0))))
  (with-compilation-unit ()
    (loop for file in '("queue"
                        "package"
                        "errors"
                        "mop"
                        "serialize"
                        "heap"
                        "object-table"
                        "schema-table"
                        "garbage-collector"
                        "cache"
                        "objects"
                        "p-btrees"
                        "index"
                        "knapsack"
                        "transactions"
                        "test")
          do (tagbody
              :retry
              (let ((lisp (make-pathname :name file
                                         :type "lisp"
                                         :defaults *knapsack-directory*)))
                (multiple-value-bind (fasl warnings failure)
                    (compile-file lisp)
                  (declare (ignore warnings))
                  (when failure
                    (restart-case
                        (error "COMPILE-FILE reported failure on ~A" lisp)
                      (retry ()
                        :report "Retry compilation"
                        (go :retry))
                      (continue ()
                        :report "Load resulting fasl anyway"
                        nil)))
                  (load fasl)))))))
