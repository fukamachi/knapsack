#|
  This file is a part of Knapsack package.
  URL: http://github.com/fukamachi/knapsack
  Copyright (c) 2006  Arthur Lemmens
  Copyright (c) 2011  Eitarow Fukamachi <e.arrows@gmail.com>

  For the full copyright and license information, please see the LICENSE.
|#

(in-package :knapsack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knapsacks: API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; open-knapsack [Function]
;; close-knapsack [Function]
;; with-knapsack [Macro]
;; current-knapsack [Function]

;; commit [Function]
;; rollback [Function]

(defgeneric add-knapsack-root (object knapsack)
  (:documentation
 "Adds an object to the root set of a knapsack."))

(defgeneric delete-knapsack-root (object knapsack)
  (:documentation
 "Delete an object from the root set of a knapsack."))

(defgeneric map-knapsack-roots (function knapsack)
  (:documentation
 "Applies a function to all objects in the root set of a knapsack."))

(defgeneric knapsack-roots (knapsack)
  (:documentation
 "Returns a list with all objects in the root set of a knapsack.  You
shouldn't modify this list."))

(defgeneric knapsack-root-p (object knapsack)
  (:documentation
   "Returns true iff OBJECT is a member of the root set of a knapsack."))

(defgeneric knapsack-cache (knapsack)
  (:documentation "Returns the cache for a knapsack."))

(defgeneric knapsack-directory (knapsack)
  (:documentation
 "Returns a pathname for the directory that contains all files of a
knapsack."))

(defgeneric knapsack-commit (knapsack)
  (:documentation
 "Ensures that all in-memory data is saved to disk."))

(defgeneric knapsack-rollback (knapsack)
  ;; DO: What does rollback mean exactly here?
  (:documentation "...."))

;;
;;  Class and slot indexing
;;

;; add-class-index (class-designator &key errorp)  [Function]
;; add-slot-index (class-designator slot index-spec &key errorp) [Function]
;; remove-class-index (class-designator &key errorp) [Function]
;; remove-slot-index (class-designator slot &key errorp) [Function]
;; map-class-indexes (function) [Function]
;; map-slot-indexes (function &key class include-subclasses) [Function]


(defgeneric knapsack-update-class-index (knapsack class)
  (:documentation
   "Compares the current class index for CLASS to the class index
that's specified in the :INDEX class option of CLASS.  An obsolete
class index (i.e. a class index that's specified anymore in the class
option) is removed, new class indexes are added."))

(defgeneric knapsack-update-slot-indexes (knapsack class)
  (:documentation
   "Compares the current slot indexes for CLASS to the slot indexes
that are specified in the slot options for the direct slots of CLASS.
Obsolete slot indexes (i.e. slot indexes that are not specified
anymore in the slot options or indexes for slots that don't exist
anymore) are removed, new slot indexes are added."))

(defgeneric knapsack-add-class-index (knapsack class-designator &key errorp))

(defgeneric knapsack-remove-class-index (knapsack class-designator
                                                  &key errorp))

(defgeneric knapsack-class-index (knapsack class-designator &key errorp)
  (:documentation "Returns the class index for a class designator."))

(defgeneric knapsack-map-class-indexes (knapsack function)
  (:documentation
   "FUNCTION must take two arguments: a class name and a class index.
It is called for all class indexes in the specified knapsack."))

(defgeneric knapsack-make-class-index (knapsack class &key index-spec)
  (:documentation
   "Creates a new class index and returns that index.  INDEX-SPEC
specifies the kind of index that must be created (if not supplied, the
knapsack's default class index spec will be used."))


(defgeneric knapsack-add-slot-index (knapsack class-designator slot index-spec
                                     unique-p &key errorp)
  (:documentation
  "Creates a new slot index for the slot designated by
CLASS-DESIGNATOR and SLOT.  The type of index is specified by
INDEX-SPEC.  Returns the new index.  Signals an error if ERRORP is T
and there already is an index for the designated slot."))

(defgeneric knapsack-remove-slot-index (knapsack class-designator slot
                                        &key errorp))



(defgeneric knapsack-slot-index (knapsack class-designator slot
                                 &key errorp include-superclasses)
  (:documentation
 "Returns the slot index for the slot specified by CLASS-DESIGNATOR
and SLOT."))


(defgeneric knapsack-map-slot-indexes (knapsack function
                                       &key class include-subclasses)
  (:documentation
   "FUNCTION must take three arguments: a class name, a slot name and
a slot index.  It is called for all slot indexes in the specified
knapsack.
  CLASS defaults to T, meaning all classes.
  INCLUDE-SUBCLASSES defaults to T."))

(defgeneric knapsack-maybe-index-changed-slot (knapsack
                                               class object slot
                                               old-value new-value
                                               old-boundp new-boundp)
  (:documentation
 "This function is called after a slot has changed.  OLD-VALUE is the
slot's value before the change, NEW-VALUE is the current value.
OLD-BOUNDP is true iff the slot was bound before the change,
NEW-BOUNDP is true iff the slot is currently bound."))

(defgeneric knapsack-maybe-index-new-object (knapsack class-designator object)
  (:documentation
 "Adds the object id of OBJECT to the class index for the class
designated by CLASS-DESIGNATOR.  If there is no such class index, it
does nothing."))

(defgeneric knapsack-map-class (knapsack class function
                                &key id-only include-subclasses)
  (:documentation
 "  FUNCTION is a unary function that gets called for all instances of
the specified class.  Unindexed classes (i.e. classes for which the
:indexed class option is nil) will be skipped.
  If ID-ONLY is T (default is NIL), the function will be called with
object ids instead of 'real' objects.  This can be handy if you want to
do more filtering before actually loading objects from disk.
  INCLUDE-SUBCLASSES defaults to T."))

(defmacro knapsack-do-class ((instance-var class
                              &key
                              (knapsack '*knapsack*)
                              id-only
                              (include-subclasses t))
                             &body body)
  "Evaluate BODY for each instance of CLASS, with INSTANCE-VAR
successively bound to each instance.  See the documentation of
KNAPSACK-MAP-CLASS for more details."
  (check-type instance-var symbol)
  `(knapsack-map-class ,knapsack ,class
                       (lambda (,instance-var) ,@body)
                       :id-only ,id-only
                       :include-subclasses ,include-subclasses))

(defgeneric knapsack-map-slot (knapsack class slot function
                              &key equal min max include-min include-max order
                              include-subclasses)
  (:documentation
 " FUNCTION is a unary function that gets called for all instances of
the specified class that have a slot value matching the EQUAL, MIN,
MAX INCLUDE-MIN and INCLUDE-MAX arguments (see the documentation of
MAP-INDEX for a description of these arguments).
  ORDER can be either :ASCENDING (default) or :DESCENDING; currently,
the specified order will be respected for instances of one class but
not across subclasses.
  If ID-ONLY is T (default is NIL), the function will be called with
object ids instead of 'real' objects.  This can be handy if you want to
do more filtering before actually loading objects from disk.
  INCLUDE-SUBCLASSES defaults to T."))

(defmacro knapsack-do-slot ((instance-var class slot
                             &rest args
                             &key (knapsack '*knapsack*)
                             equal min max include-min include-max
                             order include-subclasses)
                            &body body)
  "Evaluate BODY for each instance of CLASS where SLOT has the
specified value. INSTANCE-VAR will be bound successively to each
instance.  See the documentation of KNAPSACK-MAP-SLOT for more
details."
  (declare (ignorable equal min max include-min include-max order
                      include-subclasses))
  (check-type instance-var symbol)
  `(knapsack-map-slot ,knapsack ,class ,slot
                      (lambda (,instance-var) ,@body)
                      ,@(sans args ':knapsack)))


#+later
(defgeneric knapsack-map-objects (knapsack class-designator function
                                           slots filter order)
  (:documentation
 " Applies FUNCTION to all instances of the class designated by
CLASS-DESIGNATOR for which the criteria specified by SLOTS and
CRITERIA hold.
  SLOTS is a list of slot names.  FILTER is a filter expression that can
refer to the slot names.
  Example of a filter expression: (and (= age 20) (string= city \"Hamburg\"))
"))


(defgeneric knapsack-delete-object (knapsack object)
  (:documentation
   "Removes OBJECT from KNAPSACK, i.e. removes object from the
knapsack roots (if it is a root) and from all class and slot indexes
in which it appears."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-lock (&key (name "lock"))
  (bt:make-lock name))

(defmacro with-lock ((lock) &body body)
  `(bt:with-lock-held (,lock) ,@body))

(defun process-lock (lock)
  (bt:acquire-lock lock))

(defun process-unlock (lock)
  (bt:release-lock lock))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WITH-TRANSACTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It would be prettier if we could put this macro in TRANSACTIONS.LISP, but
;; we need it here already.

(defparameter *transaction* nil
  "The currently active transaction.")

(defmacro with-transaction ((&rest args
                             &key
                             (knapsack '(current-knapsack))
                             (inhibit-gc nil inhibit-gc-supplied-p)
                             &allow-other-keys)
                            &body body)
  (let ((committed (gensym "COMMITTED"))
        (transaction (gensym "TRANSACTION"))
        (result (gensym "RESULT")))
    `(let ((,transaction nil)
           (*collect-garbage-on-commit* (if ,inhibit-gc-supplied-p
                                            ,(not inhibit-gc)
                                            *collect-garbage-on-commit*)))
       (loop named ,transaction do
          (with-simple-restart (retry "Retry ~S" ,transaction)
            (let ((,committed nil)
                  (,result nil))
              (unwind-protect
                   (progn
                     ;; Use a local variable for the transaction so that nothing
                     ;; can replace it from underneath us, and only then bind
                     ;; it to *TRANSACTION*.
                     (setf ,transaction (transaction-start :knapsack ,knapsack
                                                           ,@(sans args :knapsack)))
                     (let ((*transaction* ,transaction))
                       (with-simple-restart (abort "Abort ~S" ,transaction)
                         (setf ,result (progn ,@body))
                         (transaction-commit ,transaction)
                         (setf ,committed t)))
                     ;; Normal exit from the WITH-SIMPLE-RESTART above -- either
                     ;; everything went well or we aborted -- the ,COMMITTED will tell
                     ;; us. In either case we jump out of the RETRY loop.
                     (return-from ,transaction (values ,result ,committed)))
                (unless ,committed
                  (transaction-rollback ,transaction)))))
            ;; Normal exit from the above block -- we selected the RETRY restart.
            ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knapsacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass knapsack ()
  ())

(defclass standard-knapsack (knapsack)
  ((cache :reader knapsack-cache)
   (directory :initarg :directory :reader knapsack-directory)
   (roots :initform '()
          :documentation "A list with the object ids of all root
objects, i.e.  the objects from which the garbage collector can reach
all live objects.")
   (roots-changed-p :initform nil :accessor roots-changed-p)
   (highest-transaction-id :initform 0
                           :accessor highest-transaction-id
                           :type integer
                           :documentation "The highest transaction ID
in the entire knapsack.  This is saved together with the roots.")
   ;; Indexes
   (class-index-table
    :documentation "The object id of a btree mapping class names to
class indexes.  Each class index contains the ids of all instances
from a class; it maps object ids to objects.")
   (slot-index-tables
    :documentation "The object id of a btree mapping class names to
slot index tables, where each slot index table is a btree mapping slot
names to slot indexes.  Each slot index maps slot values to
objects.")))

(defmethod print-object ((knapsack knapsack) stream)
  (print-unreadable-object (knapsack stream :type t :identity t)
    (format stream "in ~S with ~D root~:P"
            (knapsack-directory knapsack)
            (length (slot-value knapsack 'roots)))))

(defmethod knapsack-roots-pathname ((knapsack standard-knapsack))
  (merge-pathnames "roots" (knapsack-directory knapsack)))


(defmethod class-index-table ((knapsack standard-knapsack))
  ;; Create class-index-table if it doesn't exist yet.
  (flet ((do-it ()
           (unless (slot-boundp knapsack 'class-index-table)
             ;; Create a btree mapping class names to class
             ;; indexes.
             (let ((btree (make-instance 'btree
                                         :knapsack knapsack
                                         :key< 'string<
                                         :value= 'p-eql
                                         :unique-keys-p t
                                         :dont-index t)))
               (setf (slot-value knapsack 'class-index-table) (object-id btree)
                     (roots-changed-p knapsack) t)))
           (cache-get-object (slot-value knapsack 'class-index-table)
                             (knapsack-cache knapsack))))
    (if (current-transaction)
        (do-it)
      (with-transaction (:knapsack knapsack)
        (do-it)))))


(defmethod slot-index-tables ((knapsack standard-knapsack))
  ;; Create slot-index-tables if they don't exist yet.
  (flet ((do-it ()
           (unless (slot-boundp knapsack 'slot-index-tables)
             ;; Create a btree mapping class names to slot
             ;; index tables.
             (let ((btree (make-instance 'btree
                                         :knapsack knapsack
                                         :key< 'string<
                                         :value= 'p-eql
                                         :unique-keys-p t
                                         :dont-index t)))
               (setf (slot-value knapsack 'slot-index-tables) (object-id btree)
                     (roots-changed-p knapsack) t)))
           ;;
           (cache-get-object (slot-value knapsack 'slot-index-tables)
                             (knapsack-cache knapsack))))
    (if (current-transaction)
        (do-it)
      (with-transaction (:knapsack knapsack)
        (do-it)))))

(defmethod initialize-instance :after ((knapsack standard-knapsack)
                                       &key
                                       (cache-class 'lazy-cache)
                                       (cache-args '())
                                       &allow-other-keys)
  ;; Open cache.
  (setf (slot-value knapsack 'cache)
        (apply #'open-cache (knapsack-directory knapsack)
               :class cache-class
               :knapsack knapsack
               cache-args))
  (load-roots knapsack))

(defun load-roots (knapsack)
  ;; Read roots (i.e. object ids) from the roots file (if there is
  ;; one).  Also load the highest transaction id and the (object ids
  ;; of the) class and slot index tables.
  (let ((roots-file (knapsack-roots-pathname knapsack)))
    (when (probe-file roots-file)
      (destructuring-bind (root-list class-index slot-index
                                     &optional
                                     ;; Added in version 0.1.20.
                                     highest-transaction)
          (load-objects roots-file)
        (with-slots (roots class-index-table slot-index-tables highest-transaction-id)
            knapsack
          (setf roots root-list)
          (when class-index
            (setf class-index-table class-index))
          (when slot-index
            (setf slot-index-tables slot-index))
          (when highest-transaction
            (setf highest-transaction-id highest-transaction))))))
  knapsack)


(defun save-roots (knapsack)
  (save-objects (list (slot-value knapsack 'roots)
                      (and (slot-boundp knapsack 'class-index-table)
                           (slot-value knapsack 'class-index-table))
                      (and (slot-boundp knapsack 'slot-index-tables)
                           (slot-value knapsack 'slot-index-tables))
                      (slot-value knapsack 'highest-transaction-id))
                (knapsack-roots-pathname knapsack))
  (setf (roots-changed-p knapsack) nil))

(defun save-roots-if-necessary (knapsack)
  (when (roots-changed-p knapsack)
    (save-roots knapsack)))

(defmethod add-knapsack-root (object (knapsack standard-knapsack))
  (add-knapsack-root-id (object-id object) knapsack))

(defun add-knapsack-root-id (object-id knapsack)
  (push object-id (slot-value knapsack 'roots))
  (setf (roots-changed-p knapsack) t))

(defmethod delete-knapsack-root (object (knapsack standard-knapsack))
  (with-slots (roots)
      knapsack
    (setf roots (delete (object-id object) roots)
          (roots-changed-p knapsack) t)))

(defmethod map-knapsack-roots (function (knapsack standard-knapsack))
  (loop for root-id in (slot-value knapsack 'roots)
        do (funcall function
                    (cache-get-object root-id (knapsack-cache knapsack)))))

(defmethod knapsack-roots ((knapsack standard-knapsack))
  (let ((result '()))
    (map-knapsack-roots (lambda (root) (push root result))
                        knapsack)
    ;; We don't need to nreverse the list, because the order isn't specified.
    result))

(defmethod knapsack-root-p (object (knapsack standard-knapsack))
  (member (object-id object)
          (slot-value knapsack 'roots)))

;;
;; Opening
;;

(defparameter *knapsack-opening-lock*
  (make-lock :name "Knapsack opening lock"))

(defun open-knapsack (directory-designator
                      &rest args
                      &key
                      (class 'serial-transaction-knapsack)
                      (if-exists :overwrite)
                      (if-does-not-exist :create)
                      (cache-class 'lazy-cache)
                      (cache-args '())
                      &allow-other-keys)
  "Opens the knapsack in the directory designated by DIRECTORY-DESIGNATOR.
  :IF-DOES-NOT-EXIST can be either :CREATE (creates a new knapsack if the
it does not exist; this is the default) or :ERROR (signals an error if
the knapsack does not exist).
  :IF-EXISTS can be either :OVERWRITE (loads the knapsack if it exists;
this is the default), :SUPERSEDE (deletes the existing knapsack and creates
a new empty knapsack) or :ERROR (signals an error if the knapsack exists)."
  (declare (ignorable cache-class cache-args))
  (check-type directory-designator (or string pathname))
  (check-type if-exists (member :overwrite :supersede :error))
  (check-type if-does-not-exist (member :create :error))
  (let ((directory (if (stringp directory-designator)
                      (pathname directory-designator)
                      directory-designator)))
    (with-lock (*knapsack-opening-lock*)
      (setq *knapsack*
            (if (probe-file (merge-pathnames "roots" directory))
                ;; Knapsack already exists.
                (ecase if-exists
                  (:error
                   (error "Can't create knapsack in ~S: the directory
already seems to contain a knapsack."
                          directory))
                  (:supersede
                   ;; Remove all knapsack files from the directory.
                   (loop for file in (knapsack-files-in-directory directory)
                         do (delete-file file))
                   ;; And create a fresh knapsack.
 		   (apply #'make-instance class :directory directory args))
                  (:overwrite
                   ;; This is the normal case.
                   (apply #'make-instance class :directory directory args)))
              ;; Knapsack doesn't seem to exist.
              (ecase if-does-not-exist
                (:error
                 (error "Can't open knapsack in ~S: the knapsack roots
file is missing."
                        directory))
                (:create
                 (ensure-directories-exist directory)
                 (apply #'make-instance class :directory directory args))))))))


(defun knapsack-files-in-directory (directory-pathname)
  "Returns a list with the pathnames of all Knapsack files
in the specified directory."
  (list (merge-pathnames "roots" directory-pathname)
        (merge-pathnames "objects" directory-pathname)
        (merge-pathnames "heap" directory-pathname)
        (merge-pathnames "schemas" directory-pathname)))


(defun close-knapsack (knapsack &key (commit t))
  (when commit
    (knapsack-commit knapsack))
  ;; If :COMMIT is true, the cache and transaction handler are already
  ;; committed by the knapsack-commit, so we close them without committing.
  (close-cache (knapsack-cache knapsack) :commit nil))

;;
;; Commit
;;

(defun commit (&key (knapsack (current-knapsack)))
  (knapsack-commit knapsack))

(defmethod knapsack-commit ((knapsack standard-knapsack))
  (when (or (roots-changed-p knapsack)
            (not (slot-boundp knapsack 'class-index-table))
            (not (slot-boundp knapsack 'slot-index-tables)))
    (save-roots knapsack))
  (cache-commit (knapsack-cache knapsack)))

;;
;; Rollback
;;

(defun rollback (&key (knapsack (current-knapsack)))
  (knapsack-rollback knapsack))

(defmethod knapsack-rollback ((knapsack standard-knapsack))
  ;; Rollback the cache.
  (cache-rollback (knapsack-cache knapsack))
  ;; Rollback the roots by loading them back from file.
  (load-roots knapsack)
  (setf (roots-changed-p knapsack) nil))

;;
;; Some small stuff
;;

(defmacro with-knapsack ((knapsack directory &rest args) &body body)
   `(let* ((*knapsack* *knapsack*)
           (,knapsack (open-knapsack ,directory ,@args)))
      (unwind-protect (progn ,@body)
        (close-knapsack ,knapsack))))


(defun test-garbage-collector (knapsack)
  (collect-garbage (heap (knapsack-cache knapsack))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indexing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod knapsack-update-class-index ((knapsack standard-knapsack)
                                        (class persistent-class))
  (let ((old-index (knapsack-class-index knapsack class :errorp nil))
        (needs-index-p (class-index class)))
    (cond ((and old-index (not needs-index-p))
           (knapsack-remove-class-index knapsack class :errorp t))
          ((and (not old-index) needs-index-p)
           ;; Create a class index now.
           ;; NOTE: If there are existing instances of this class,
           ;; they're *not* automatically indexed at this point.
           ;; (In fact, the only way to do this would be to iterate
           ;; over *all* objects in the knapsack, which would be rather
           ;; expensive.  Then again, it's exactly what the garbage
           ;; collector does anyway, so it may be an option to have the
           ;; garbage collector index them automatically.  But I'm not
           ;; sure if that's a good idea.)
           (knapsack-add-class-index knapsack class :errorp t))
          (t
           ;; We don't need to change anything
           :no-change))))



(defmethod knapsack-update-slot-indexes ((knapsack standard-knapsack)
                                         (class persistent-class))
  (let ((direct-slots (class-direct-slots class))
        (indexed-slot-names (knapsack-indexed-slots-for-class knapsack class)))
    ;; Remove indexes for slots that don't exist anymore.
    (loop for slot-name in indexed-slot-names
          unless (find slot-name direct-slots :key #'slot-definition-name)
          do (knapsack-remove-slot-index knapsack class slot-name :errorp nil))
    ;; Update indexes for the current set of direct slots.
    (dolist (slot direct-slots)
      (let ((index-spec (and (slot-persistence slot)
                             (or (find-index-spec (slot-index slot) :errorp nil)
                                 (slot-index slot))))
            (unique-p (slot-unique slot))
            (slot-name (slot-definition-name slot)))
        (let* ((current-index (knapsack-slot-index knapsack class slot-name
                                                   :errorp nil
                                                   :include-superclasses nil))
               (current-index-spec (and current-index (index-spec current-index)))
               (current-unique-p (and current-index (index-unique-keys-p current-index))))
          (cond ((and (index-spec-equal index-spec current-index-spec)
                      (eql unique-p current-unique-p))
                 ;; We keep the same index: no change needed.
                 :no-change)
                ((and current-index-spec (null index-spec))
                 ;; The index is not wanted anymore: remove it.
                 (knapsack-remove-slot-index knapsack class slot :errorp t))
                ((and (null current-index-spec) index-spec)
                 ;; We didn't have an index but we need one now: add one.
                 (add-and-fill-slot-index knapsack class slot index-spec unique-p))
                ((and current-index-spec index-spec)
                 ;; We have an index but need a different one now.
                 (replace-slot-index knapsack class slot index-spec unique-p))))))))


(defun add-and-fill-slot-index (knapsack class slot index-spec unique-p)
  ;; We didn't have an index but we need one now: add one.
  (let ((index (knapsack-add-slot-index knapsack class slot index-spec unique-p
                                        :errorp t))
        (slot-name (slot-definition-name slot)))
    ;; Index all instances for the new index.
    ;; NOTE: This will only work if the class is indexed, otherwise there is no
    ;; affordable way to find all instances of the class.
    (when (class-index class)
      (knapsack-map-class knapsack class
                          (lambda (object)
                            (when (slot-boundp object slot-name)
                              (index-insert index (slot-value object slot-name)
                                            object)))))))


(defun replace-slot-index (knapsack class slot index-spec unique-p)
  ;; We have an index but need a different one now.  This requires
  ;; some care because we need to re-index all objects from the old
  ;; index.
  (let ((current-index (knapsack-slot-index knapsack class slot))
        (new-index (knapsack-add-slot-index knapsack class slot
                                            index-spec
                                            unique-p
                                            :errorp nil)))
    ;; Re-index all objects for the new index.
    ;; DO: This re-indexing can cause an error (e.g. if the old
    ;; index has non-unique keys, the new index has unique keys
    ;; and some keys occur more than once).  We need to handle
    ;; that error here and offer some decent restarts (e.g.
    ;; remove the index entirely, or go back to the old index).
    (map-index current-index
               (lambda (slot-value object)
                 (index-insert new-index slot-value object)))
    ;; We don't need to remove the old index explicitly, because
    ;; KNAPSACK-ADD-SLOT-INDEX already did that for us.
    ))

(defun find-old-index-spec (slot-name old-slots)
  (let ((slot (find slot-name old-slots :key #'slot-definition-name)))
    (and slot
         (with-slots (index unique)
             slot
           (values (or (find-index-spec index :errorp nil) index)
                   unique)))))

;;
;; Some simple dispatchers.
;;

;; Q: Are these really necessary?

(defun add-class-index (class-designator &key errorp)
  (knapsack-add-class-index (current-knapsack) class-designator
                            :errorp errorp))

(defun add-slot-index (class-designator slot index-spec &key (errorp nil))
  (knapsack-add-slot-index (current-knapsack) class-designator slot index-spec
                           :errorp errorp))

(defun remove-class-index (class-designator &key (errorp nil))
  (knapsack-remove-class-index (current-knapsack) class-designator
                               :errorp errorp))

(defun remove-slot-index (class-designator slot &key (errorp nil))
  (knapsack-remove-slot-index (current-knapsack) class-designator slot
                              :errorp errorp))

(defun map-class-indexes (function)
  (knapsack-map-class-indexes (current-knapsack) function))

(defun map-slot-indexes (function &key (class t) (include-subclasses t))
  (knapsack-map-slot-indexes (current-knapsack) function
                             :class class
                             :include-subclasses include-subclasses))

;;
;; Class indexes
;;

(defmethod knapsack-add-class-index ((knapsack standard-knapsack) class
                                     &key (errorp nil))
  ;; Create and add a class index to the class index table.
  (unless (symbolp class)
    (setq class (class-name class)))
  (when (and errorp (btree-search (class-index-table knapsack) class
                                  :errorp nil :default-value nil))
    (simple-knapsack-error "Class index for ~S already exists in ~A."
                           class
                           knapsack))
  (let ((index (knapsack-make-class-index knapsack class)))
    (btree-insert (class-index-table knapsack) class index
                  :if-exists :overwrite)
    index))

(defmethod knapsack-make-class-index
           ((knapsack standard-knapsack) class
            &key (index-spec '(btree :key< < :value= p-eql)))
  ;; A class index maps object ids to objects.
  (declare (ignore class))
  (make-index index-spec t))

(defmethod knapsack-remove-class-index ((knapsack standard-knapsack) class
                                        &key (errorp nil))
  (unless (symbolp class)
    (setq class (class-name class)))
  (handler-bind ((btree-deletion-error
                  ;; Translate a btree error to something that makes more sense
                  ;; in this context.
                  (lambda (error)
                    (declare (ignore error))
                    (simple-knapsack-error "Class index for ~S doesn't exist in ~A."
                                           class
                                           knapsack))))
    (btree-delete-key (class-index-table knapsack) class
                      :if-does-not-exist (if errorp :error :ignore))))


(defmethod knapsack-map-class-indexes (knapsack function)
  (map-btree (class-index-table knapsack) function))

(defmethod knapsack-class-index ((knapsack standard-knapsack) class
                                 &key (errorp nil))
  (unless (symbolp class)
    (setq class (class-name class)))
  (handler-bind ((btree-search-error
                  ;; Translate a btree error to something that makes more sense
                  ;; in this context.
                  (lambda (error)
                    (declare (ignore error))
                    (simple-knapsack-error "Can't find class index for ~S in ~A."
                                           class
                                           knapsack))))
    (btree-search (class-index-table knapsack) class
                  :errorp errorp
                  :default-value nil)))


(defmethod knapsack-maybe-index-new-object ((knapsack standard-knapsack)
                                            class object)
  (let ((index (knapsack-class-index knapsack class :errorp nil)))
    (when index
      (index-insert index (object-id object) object
                    :if-exists :error))))


(defmethod knapsack-map-class ((knapsack standard-knapsack) class function
                               &key (id-only nil) (include-subclasses t))
  ;; EFFICIENCY: Follow Sean Ross' suggestion and implement ID-ONLY
  ;; by defining a function MAP-INDEX-KEYS and then calling
  ;; that function here (so that we don't need to load any objects
  ;; that we don't want to load yet).
  (let ((visited-p (make-hash-table)))
    (labels ((map-instances (class)
               (let ((index (knapsack-class-index knapsack class :errorp nil)))
                 (when index
                   (map-index index
                              (lambda (id object)
                                (if id-only
                                    (funcall function id)
                                  (funcall function object))))
                   (setf (gethash class visited-p) t))
                 (when include-subclasses
                   (loop for class in (class-direct-subclasses
                                       (if (symbolp class)
                                           (find-class class)
                                         class))
                         unless (gethash class visited-p)
                         do (map-instances class))))))
      (map-instances class))))

;;
;; Slot indexing
;;

(defmethod knapsack-add-slot-index ((knapsack standard-knapsack)
                                    class slot index-spec unique-p
                                    &key (errorp nil))
  (unless (symbolp class)
    (setq class (class-name class)))
  (unless (symbolp slot)
    (setq slot (slot-definition-name slot)))
  ;; Find the slot index table for CLASS, create a slot index and add that
  ;; index to the table.
  (let* ((slot-index-tables (slot-index-tables knapsack))
         (slot-index-table
          (or (btree-search slot-index-tables class :errorp nil)
              (let ((table (make-instance 'btree
                                          :key< 'string<
                                          :value= 'p-eql
                                          :unique-keys-p t)))
                (btree-insert slot-index-tables class table :if-exists :error)
                table)))
         (new-slot-index (make-index index-spec unique-p)))
    (handler-bind ((btree-key-already-present-error
                    (lambda (error)
                      (declare (ignore error))
                      (simple-knapsack-error "Slot index for slot ~S of class ~S
already exists in ~S."
                                             slot
                                             class
                                             knapsack))))
      (btree-insert slot-index-table slot new-slot-index
                    :if-exists (if errorp :error :overwrite)))
    new-slot-index))


(defmethod knapsack-remove-slot-index (knapsack class slot &key (errorp nil))
  (unless (symbolp class)
    (setq class (class-name class)))
  (unless (symbolp slot)
    (setq slot (slot-definition-name slot)))
  (flet ((oops (error)
           (declare (ignore error))
           (simple-knapsack-error "Attempt to remove non-existing slot
index for slot ~S of class ~S in ~S."
                                  slot
                                  class
                                  knapsack)))
    ;; Return the slot name if everything went fine; otherwise, return
    ;; NIL (or signal an error).
    (and (handler-bind ((btree-search-error #'oops))
           (let ((slot-index-table (btree-search (slot-index-tables knapsack) class
                                                 :errorp errorp)))
             (and slot-index-table
                  (handler-bind ((btree-deletion-error #'oops))
                    (btree-delete-key slot-index-table slot
                                      :if-does-not-exist (if errorp :error :ignore))))))
         slot)))


(defmethod knapsack-map-slot-indexes ((knapsack standard-knapsack) function
                                      &key (class t) (include-subclasses t))
  (if (eql class t)
      (map-btree (slot-index-tables knapsack)
                 (lambda (class slot-index-table)
                   (map-btree slot-index-table
                              (lambda (slot slot-index)
                                (funcall function class slot slot-index)))))
    (let ((visited-p (make-hash-table)))
      (labels ((map-indexes (class)
                 (unless (gethash class visited-p)
                   (let ((slot-index-table (btree-search (slot-index-tables knapsack)
                                                         (class-name class)
                                                         :errorp nil)))
                     (when slot-index-table
                       (map-btree slot-index-table
                                  (lambda (slot slot-index)
                                    (funcall function (class-name class)
                                             slot
                                             slot-index)))))
                   (setf (gethash class visited-p) t)
                   (when include-subclasses
                     (mapc #'map-indexes
                           (class-direct-subclasses class))))))
        (map-indexes (if (symbolp class) (find-class class) class))))))


(defmethod knapsack-maybe-index-changed-slot ((knapsack standard-knapsack)
                                              class object slot
                                              old-value new-value
                                              old-boundp new-boundp)
  ;; SLOT is a slot-definition, not a slot name.
  (when (slot-index slot)
    (let ((index (knapsack-slot-index knapsack class slot
                                      :errorp nil
                                      :include-superclasses t)))
      (when index
        (when old-boundp
          (index-delete index old-value object
                        :if-does-not-exist :ignore))
        (when new-boundp
          (index-insert index new-value object
                        :if-exists (if (slot-unique slot)
                                       :error
                                     :overwrite)))))))


(defmethod knapsack-slot-index ((knapsack standard-knapsack) class slot
                                &key (errorp nil) (include-superclasses nil))
  (unless (symbolp class)
    (setq class (class-name class)))
  (unless (symbolp slot)
    (setq slot (slot-definition-name slot)))
  (let ((slot-index-tables (slot-index-tables knapsack)))
    (flet ((find-index (class)
             (let ((slot-index-table (btree-search slot-index-tables class
                                                   :errorp nil)))
 	       (and slot-index-table
                    (btree-search slot-index-table slot :errorp nil)))))
      (or (find-index class)
          (and include-superclasses
               (loop for superclass in (class-precedence-list (find-class class))
                     thereis (find-index (class-name superclass))))
          (and errorp
               (simple-knapsack-error
                "Can't find slot index for slot ~S of class ~S in ~S."
                slot
                class
                knapsack))))))


(defmethod knapsack-map-slot ((knapsack standard-knapsack) class slot function
                              &key min max include-min include-max
                              (equal nil equal-supplied)
                              (order :ascending) (include-subclasses t))
  (let ((visited-p (make-hash-table)))
    (labels ((map-slot (class)
               (let ((index (knapsack-slot-index knapsack class slot
                                                 :errorp nil)))
                 (when index
                   ;; The index maps slot values to objects.
                   (apply #'map-index
                          index
                          (lambda (slot-value object)
                            (declare (ignore slot-value))
                            (funcall function object))
                          :min min
                          :max max
                          :include-min include-min
                          :include-max include-max
                          :order order
                          (if equal-supplied (list :equal equal) '()))
                   (setf (gethash class visited-p) t))
                 (when include-subclasses
                   (loop for class in (class-direct-subclasses
                                       (if (symbolp class)
                                           (find-class class)
                                         class))
                         unless (gethash class visited-p)
                         do (map-slot class))))))
      (map-slot (if (symbolp class) (find-class class) class)))))


(defun knapsack-indexed-slots-for-class (knapsack class)
  "Returns a list with the names of the indexed direct slots of CLASS."
  (unless (symbolp class)
    (setq class (class-name class)))
  (let ((result '()))
    (knapsack-map-slot-indexes knapsack
                               (lambda (class-name slot-name slot-index)
                                 (declare (ignore slot-index))
                                 (when (eql class-name class)
                                   (push slot-name result))))
    result))


;;
;; Debugging
;;

(defun knapsack-list-slot-indexes (knapsack)
  (let ((result '()))
    (knapsack-map-slot-indexes knapsack
                               (lambda (class-name slot-name slot-index)
                                 (declare (ignore slot-index))
                                 (push (cons class-name slot-name)
                                       result)))
    result))

(defun knapsack-list-class-indexes (knapsack)
  (let ((result '()))
    (knapsack-map-class-indexes knapsack
                                (lambda (class-name index)
                                  (declare (ignore index))
                                  (push class-name result)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deleting objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod knapsack-delete-object ((knapsack standard-knapsack) object)
  (let ((class-name (class-name (class-of object))))
    ;; Remove object from class index if necessary.
    (let ((class-index (knapsack-class-index knapsack (class-of object)
                                             :errorp nil)))
      (when class-index
        (index-delete class-index (object-id object) object)))
    ;; Remove object from slot indexes if necessary.
    (let ((indexed-slot-names (knapsack-indexed-slots-for-class knapsack
                                                                (class-of object))))
      (loop for slot-name in indexed-slot-names do
            (index-delete (knapsack-slot-index knapsack class-name slot-name)
                          (slot-value object slot-name)
                          object
                          :if-does-not-exist :ignore)))
    ;; Remove object from roots if necessary.
    (when (knapsack-root-p object knapsack)
      (delete-knapsack-root object knapsack))))


(defun knapsack-delete-objects (knapsack objects)
  (dolist (object objects)
    (knapsack-delete-object knapsack object)))
