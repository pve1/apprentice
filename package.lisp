;;;; Requires package.seed

(package.seed:define-package :slime-apprentice)

(in-package :slime-apprentice)

(define-symbol-macro [*]
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (setf *readtable* (capitalized-export:make-capitalized-export-readtable))))
