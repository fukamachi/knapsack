#|
  This file is a part of Knapsack package.
  URL: http://github.com/fukamachi/knapsack
  Copyright (c) 2006  Arthur Lemmens
  Copyright (c) 2011  Eitarow Fukamachi <e.arrows@gmail.com>

  For the full copyright and license information, please see the LICENSE.
|#

(in-package :knapsack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Import/export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The contents of a knapsack can be exported to a single file.  The file will
;; contain enough information to reconstruct the original knapsack objects.
;; Knapsack export files use a relatively simple s-expression format.
;;
;; There are two reasons for exporting a knapsack:
;; - backup
;;   The export file has a simple format, so it's a lot less sensitive
;;   to data corruption bugs.
;; - migration
;;   Export files can be imported by newer versions of Knapsack.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Import/export API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric export-knapsack (knapsack pathname)
  (:documentation "Export all objects in a knapsack to a file.  The
resulting file can be imported by newer versions of Knapsack."))

(defun import-knapsack (pathname directory-designator
                        &rest args
                        &key (if-exists :error)
                        &allow-other-keys)
  "Creates a new knapsack in the directory specified by
DIRECTORY-DESIGNATOR, opens the new knapsack and imports all objects
that were exported to the file specified by PATHNAME."
  (declare (ignore pathname directory-designator if-exists args))
  (error "Not implemented yet"))
