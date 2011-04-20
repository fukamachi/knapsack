#|
  This file is a part of Knapsack package.
  URL: http://github.com/fukamachi/knapsack
  Copyright (c) 2006  Arthur Lemmens
  Copyright (c) 2011  Eitarow Fukamachi <e.arrows@gmail.com>

  For the full copyright and license information, please see the LICENSE.
|#

(in-package :knapsack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knapsack errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition knapsack-error (error)
  ((knapsack :initarg :knapsack :initform (current-knapsack)
             :reader knapsack)))

(defmethod print-object ((error knapsack-error) stream)
  (format stream "Knapsack error in ~A." (knapsack error)))

(defun knapsack-error (class &rest args)
  (apply #'error class
         :knapsack (current-knapsack)
         args))

;;
;; Transaction conflict
;;

(define-condition transaction-conflict (knapsack-error)
  ((transaction :initarg :transaction :initform (current-transaction)
                :reader transaction)
   (old-transaction :initarg :old-transaction
                    :initform (error "OLD-TRANSACTION initarg required
for transaction-conflict.")
                    :reader old-transaction)
   (object-id :initarg :object-id
              :initform (error "OBJECT-ID initarg required for
transaction-conflict.")
              :reader object-id)))

(defmethod print-object :after ((error transaction-conflict) stream)
  (format stream "~&~A can't modify object #~D, because ~A already
modified it and hasn't committed yet."
          (transaction error)
          (object-id error)
          (old-transaction error)))

;;
;; Simple knapsack error
;;

(define-condition simple-knapsack-error (knapsack-error simple-error)
  ())

(defmethod print-object :after ((error simple-knapsack-error) stream)
  (format stream "~&~A~%"
          (apply #'format nil (simple-condition-format-control error)
                 (simple-condition-format-arguments error))))

(defun simple-knapsack-error (format-string &rest format-args)
  (knapsack-error 'simple-knapsack-error
                  :format-control format-string
                  :format-arguments format-args))


;;
;; Internal knapsack errors
;;

(define-condition internal-knapsack-error (knapsack-error simple-error)
  ())

(defmethod print-object :after ((error internal-knapsack-error) stream)
  (format stream "~&Internal error: ~A~%"
          (apply #'format nil (simple-condition-format-control error)
                 (simple-condition-format-arguments error))))

(defun internal-knapsack-error (format-string &rest format-args)
  (knapsack-error 'internal-knapsack-error
                  :format-control format-string
                  :format-arguments format-args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition slot-error (knapsack-error)
  ;; Q: Maybe this should inherit from CELL-ERROR??
  ((object :initarg :object :reader slot-error-object)
   (slot-name :initarg :slot-name :reader slot-error-name)
   (value :initarg :value :reader slot-error-value)))

(define-condition duplicate-slot-value (slot-error)
  ((other-object :initarg :other-object
                 :reader slot-error-other-object)))

(defmethod print-object :after ((error duplicate-slot-value) stream)
  (format stream
          "Attempt to assign the value ~S to the unique slot ~S of ~S. ~
The value is already present in ~S."
          (slot-error-value error)
          (slot-error-name error)
          (slot-error-object error)
          (slot-error-other-object error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun not-implemented (operator)
  (error "~S not implemented for ~A" operator (lisp-implementation-type)))
