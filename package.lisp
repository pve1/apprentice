
(defpackage #:slime-apprentice
  (:use #:cl)
  (:export "*APPRENTICE*"
           "*BUFFER-CONTEXT*"
           "*FORCE-RETURN-DESCRIPTION*"
           "*MAX-DESCRIPTION-SIZE*"
           "DESCRIBE-WITH-APPRENTICE"
           "RESOLVE-SYMBOL"
           "SYMBOL-DESCRIPTION"
           "PRESENTATION-DESCRIPTION"
           "EVAL-WITH-APPRENTICE"
           "READ-FROM-STRING-WITH-APPRENTICE"
           "READ-PACKAGE-DESIGNATOR"
           "FORM-DESCRIPTION"
           "DESCRIBE-FORM-WITH-APPRENTICE"

           "DESCRIBE-APPRENTICE"
           "VALUE-APPRENTICE"
           "*CACHING-APPRENTICE-DEFAULT-UPDATE-INTERVAL*"
           "CACHING-APPRENTICE"
           "APROPOS-APPRENTICE"
           "GREP-APPRENTICE"
           "APPRENTICE-GATHERING"
           "APPRENTICE-GATHERING-DIVIDER"))

(in-package :slime-apprentice)
