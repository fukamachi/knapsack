#|
  This file is a part of Knapsack package.
  URL: http://github.com/fukamachi/knapsack
  Copyright (c) 2006  Arthur Lemmens
  Copyright (c) 2011  Eitarow Fukamachi <e.arrows@gmail.com>

  For the full copyright and license information, please see the LICENSE.
|#

(in-package :cl-user)
(defpackage knapsack-asd
  (:use :cl :asdf))
(in-package :knapsack-asd)

(defsystem knapsack
  :version "0.1.20"
  :serial t
  :depends-on (:bordeaux-threads
               :closer-mop)
  :components ((:module "src"
                :serial t
                :components ((:file "queue")
                             (:file "package")
                             (:file "errors")
                             (:file "mop")
                             (:file "serialize" )
                             (:file "heap")
                             (:file "object-table")
                             (:file "schema-table")
                             (:file "garbage-collector")
                             (:file "cache")
                             (:file "objects")
                             (:file "p-btrees")
                             (:file "index")
                             (:file "knapsack")
                             (:file "transactions")
                             (:file "import-export"))))
  :description "a flexible, light weight, open source persistence library"
  :long-description
  #.(let ((path (merge-pathnames
                 #p"README.markdown"
                 (or *load-pathname* *compile-file-pathname*))))
      (when (probe-file path)
        (with-open-file (stream path :direction :input)
          (let ((seq (make-array (file-length stream)
                                 :element-type 'character
                                 :fill-pointer t)))
            (setf (fill-pointer seq) (read-sequence seq stream))
            seq)))))
