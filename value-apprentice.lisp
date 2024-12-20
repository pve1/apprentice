;;;; Requires
;;;;   apprentice

(in-package :apprentice) cx

(defclass Value-apprentice ()
  ())

(defmethod describe-with-apprentice ((ap value-apprentice)
                                     (object symbol)
                                     stream)
  (cond ((and (not (keywordp object))
              (boundp object))
         (format stream "Value description:~%~%")
         (describe (symbol-value object) stream)
         t)))

