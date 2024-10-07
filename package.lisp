;;;; Requires
;;;;   package.seed
;;;;   capitalized-export

(package.seed:define-package :apprentice)

(in-package :apprentice)

(define-symbol-macro cx
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (setf *readtable* (capitalized-export:make-capitalized-export-readtable))))
