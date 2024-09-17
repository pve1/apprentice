(in-package :slime-apprentice)

(defclass value-apprentice ()
  ())

(defmethod describe-with-apprentice ((appr value-apprentice)
                                     (object symbol)
                                     stream)
  (cond ((and (not (keywordp object))
              (boundp object))
         (format stream "Value description:~%~%")
         (describe (symbol-value object) stream)
         t)))
