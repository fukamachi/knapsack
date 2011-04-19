(in-package :knapsack-test-schema-update)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schema updates and UPDATE-INSTANCE-FOR-REDEFINED-CLASS, part 1 of 3
;;;
;;; After compiling and loading this file, evaluate:
;;; - (in-package :knapsack-test-schema-update)
;;; - (test-1)
;;;
;;; Then move on to test-schema-update-1b.lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *names* '(john dick mary jane peter ronald))

;;
;; Initial class definition of PERSON
;;

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *dir* #P"/tmp/knapsack/schema-update/")
  
  (with-knapsack (rs *dir* :if-exists :supersede)
    (with-transaction ()
      
      (defclass person ()
        ((name :initarg :name
               :initform (elt *names* (random (length *names*)))
               :reader name)
         (age :initarg :age
              :initform (random 100)
              :reader age))
        (:metaclass persistent-class)
        (:index t)))))


(defmethod print-object ((person person) stream)
  (print-unreadable-object (person stream :type t)
    (format stream "#~D ~A with age ~D"
            (object-id person)
            (name person)
            (age person))))


(defun test-1 ()
  ;; Create some persons.
  (with-knapsack (rs *dir*)
    (with-transaction ()
      (loop repeat 10
            do (make-instance 'person))))
  ;; Show them.
  (with-knapsack (rs *dir*)
    (with-transaction ()
      (knapsack-map-class rs 'person #'print))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sample output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

#<PERSON #12 JOHN with age 77> 
#<PERSON #22 DICK with age 39> 
#<PERSON #24 JOHN with age 95> 
#<PERSON #26 PETER with age 41> 
#<PERSON #28 JANE with age 17> 
#<PERSON #30 JOHN with age 75> 
#<PERSON #32 PETER with age 88> 
#<PERSON #34 DICK with age 11> 
#<PERSON #36 MARY with age 49> 
#<PERSON #38 RONALD with age 72> 

|#
