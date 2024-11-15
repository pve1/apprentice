;;;; Requires
;;;;   capitalized-export

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :apprentice)
    (make-package :apprentice :use '(:cl))))

(in-package :apprentice)

(define-symbol-macro cx
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (setf *readtable* (capitalized-export:make-capitalized-export-readtable))))
