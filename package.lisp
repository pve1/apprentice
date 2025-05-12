;;;; Requires
;;;;   capitalized-export

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :apprentice)
    (make-package :apprentice :use '(:cl))))

(in-package :apprentice)

(define-symbol-macro cx
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (setf *readtable* (capitalized-export:make-capitalized-export-readtable))))

(defmacro export-capitalized-from (package-name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (find-package ',package-name)
       (make-package ',package-name :use '()))
     (let ((scanner (make-instance 'capitalized-export:default-scanner
                      :export-from-package ',package-name
                      :read-into-package *package*)))
       (setf *readtable* (capitalized-export:make-capitalized-export-readtable
                          :scanner scanner)))))
