(in-package :slime-apprentice)

(defclass describe-apprentice ()
  ())

(defmethod describe-with-apprentice ((appr describe-apprentice)
                                     object
                                     stream)
  (call-next-method))
