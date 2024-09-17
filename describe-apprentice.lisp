;;;; Requires
;;;;   slime-apprentice

(in-package :slime-apprentice) cx

(defclass Describe-apprentice ()
  ())

(defmethod describe-with-apprentice ((appr describe-apprentice)
                                     object
                                     stream)
  (call-next-method))
