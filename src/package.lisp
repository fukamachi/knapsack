#|
  This file is a part of Knapsack package.
  URL: http://github.com/fukamachi/knapsack
  Copyright (c) 2006  Arthur Lemmens
  Copyright (c) 2011  Eitarow Fukamachi <e.arrows@gmail.com>

  For the full copyright and license information, please see the LICENSE.
|#

(defpackage knapsack
  (:nicknames :ks)

  (:use :cl
        :queue)

  (:import-from :closer-mop

                :validate-superclass
                :direct-slot-definition-class
                :effective-slot-definition-class
                :compute-effective-slot-definition
                :compute-class-precedence-list

                :class-slots
                :class-direct-slots
                :class-direct-subclasses
                :class-precedence-list
                :finalize-inheritance

                :slot-definition-name
                :slot-definition-initargs
                :slot-definition-readers
                :slot-definition-writers
                :slot-definition-allocation
                :slot-definition-type
                :slot-definition-initfunction
                :slot-boundp-using-class
                :slot-value-using-class

                :standard-direct-slot-definition
                :standard-effective-slot-definition)

  (:export

   ;; Cache
   #:cache #:standard-cache
   #:open-cache #:close-cache #:with-cache
   #:cache-size #:cache-count
   #:cache-create-object #:cache-get-object #:cache-touch-object
   #:cache-commit #:cache-rollback #:cache-recover
   #:open-transaction #:close-transaction #:map-transactions

   ;; MOP related
   #:persistent-class
   #:update-persistent-instance-for-redefined-class

   ;; Objects
   #:persistent-object
   #:persistent-data #:persistent-array #:persistent-cons
   #:object-id
   #:p-cons #:p-array
   #:p-eql
   #:p-car #:p-cdr #:p-list #:p-last
   #:p-endp #:p-consp
   #:p-caar #:p-cadr #:p-cdar #:p-cddr
   #:unwrap-persistent-list
   #:p-mapcar #:p-mapc #:p-maplist #:p-mapl
   #:p-member-if
   #:p-pop #:p-push
   #:p-make-array #:p-aref #:p-array-dimensions
   #:p-length #:p-find #:p-replace #:p-delete-if #:p-position

   ;; Heaps
   #:heap #:free-list-heap #:mark-and-sweep-heap #:simple-free-list-heap
   #:open-heap #:close-heap
   #:heap-stream #:heap-end

   ;; Knapsacks
   #:*knapsack*
   #:open-knapsack #:close-knapsack #:with-knapsack #:current-knapsack
   #:knapsack #:standard-knapsack
   #:knapsack-cache
   #:knapsack-directory
   #:knapsack-commit #:knapsack-rollback
   #:add-knapsack-root #:map-knapsack-roots #:knapsack-roots
   #:commit #:rollback

   ;; Class and slot indexing
   #:add-class-index #:add-slot-index
   #:remove-class-index #:remove-slot-index
   #:map-class-indexes #:map-slot-indexes
   #:knapsack-add-class-index #:knapsack-add-slot-index
   #:knapsack-make-class-index
   #:knapsack-remove-class-index #:knapsack-remove-slot-index
   #:knapsack-class-index #:knapsack-slot-index
   #:knapsack-map-class-indexes #:knapsack-map-slot-indexes
   #:knapsack-maybe-index-changed-slot #:knapsack-maybe-index-new-object
   #:knapsack-map-class #:knapsack-map-slot
   #:knapsack-do-class #:knapsack-do-slot
   #:knapsack-delete-object

   ;; Transactions
   #:current-transaction

   #:transaction-start #:transaction-commit #:transaction-rollback
   #:with-transaction #:*transaction*
   #:transaction #:standard-transaction
   #:transaction-start-1 #:transaction-commit-1
   #:transaction-id

   ;; Conditions
   #:knapsack-error #:simple-knapsack-error #:transaction-conflict
   #:internal-knapsack-error
   #:duplicate-slot-value #:slot-error

   ;; Indexes
   #:map-index #:index-insert #:index-delete #:make-index
   #:define-index-spec #:find-index-spec

   ;; Btrees
   #:btree
   #:btree-key< #:btree-key<= #:btree-key= #:btree-key>= #:btree-key>
   #:btree-value=
   #:btree-max-node-size #:btree-unique-keys-p
   #:btree-key-type #:btree-value-type
   #:btree-node-class #:btree-node
   #:btree-nr-keys #:btree-nr-values
   ;; Functions
   #:btree-search #:btree-insert #:btree-delete #:btree-delete-key
   #:map-btree #:map-btree-keys
   ;; Conditions
   #:btree-error #:btree-search-error #:btree-insertion-error
   #:btree-key-already-present-error #:btree-type-error
   #:btree-error-btree #:btree-error-key #:btree-error-value))
