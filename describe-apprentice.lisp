;;;; Requires
;;;;   "apprentice"
;;;;   "buttons"
;;;;   "temporary-apprentice"

(in-package :apprentice) cx

(defclass Describe-apprentice ()
  ())

(defmethod describe-with-apprentice ((ap describe-apprentice)
                                     object
                                     stream)
  (flet ((space ()
           (princ " " stream)))
    (describe object stream)
    (when (symbolp object)
      (terpri stream)
      ;; Unintern, shadow
      (multiple-value-bind (sym state)
          (find-symbol (symbol-name object) *package*)
        (cond ((and (eq sym object)
                    (not (eq :inherited state)))
               (put-lisp-button-here ap
                                     "[UNINTERN]"
                                     `(unintern ',object
                                                (find-package
                                                 ,(package-name *package*)))
                                     :stream stream
                                     :redisplay t))
              ((and (eq sym object)
                    (eq :inherited state))
               (space)
               (put-lisp-button-here
                ap
                "[SHADOW]"
                `(shadow ',object (find-package
                                   ,(package-name *package*)))
                :stream stream))))
      ;; Export
      (multiple-value-bind (sym state)
          (find-symbol (symbol-name object) *package*)
        (cond ((and (eq object sym)
                    (eq state :external))
               (space)
               (put-lisp-button-here
                ap
                "[UNEXPORT]"
                `(unexport ',object (find-package ,(package-name *package*)))
                :stream stream
                :redisplay t))
              ((eq object sym)
               (space)
               (put-lisp-button-here
                ap
                "[EXPORT]"
                `(export ',object (find-package ,(package-name *package*)))
                :stream stream
                :redisplay t))))
      ;; Makunbound
      (when (and (boundp object)
                 (not (keywordp object)))
        (space)
        (put-lisp-button-here ap
                              "[MAKUNBOUND]"
                              `(makunbound ',object)
                              :stream stream
                              :redisplay t))
      ;; Fmakunbound
      (when (fboundp object)
        (space)
        (put-lisp-button-here ap
                              "[FMAKUNBOUND]"
                              `(fmakunbound ',object)
                              :stream stream
                              :redisplay t))
      ;; Methods
      (when (and (fboundp object)
                 (typep (fdefinition object) 'generic-function)
                 (closer-mop:generic-function-methods
                  (fdefinition object)))
        (space)
        (put-lisp-button-here
         ap
         "[METHODS]"
         `(set-temporary-apprentice 'method-apprentice
                                    (lambda (ap obj str)
                                      (terpri str)
                                      (describe-with-apprentice ap
                                                                obj
                                                                str)))
         :stream stream
         :redisplay t))
      (terpri stream))
    t))
