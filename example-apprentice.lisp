;;;; Requires
;;;;   apprentice
;;;;   closer-mop
;;;;   "apprentice-gathering"
;;;;   "apropos-apprentice"
;;;;   "grep-apprentice"
;;;;   "describe-apprentice"
;;;;   "value-apprentice"
;;;;   "history-apprentice"
;;;;   "method-apprentice"
;;;;   "toplevel-apprentice"
;;;;   "suggest-apprentice"
;;;;   "buttons"

(in-package :apprentice)

(defclass example-apprentice (apprentice-gathering)
  ((toplevel :initarg :toplevel
             :accessor toplevel
             :initform nil)
   (state :initarg :state
          :accessor state
          :initform nil)))

(defmethod apprentices ((ap example-apprentice))
  (if (eq (state ap) 'toplevel)
      (toplevel ap)
      (call-next-method)))

(defun example-describe-package-maybe (object stream)
  (when (and (or (keywordp object)
                 (and (symbolp object)
                      (null (symbol-package object))))
             (find-package object))
    (let ((*standard-output* stream)
          (package (find-package object)))
      (format t "~A names a package: " object)
      (put-lisp-button-here *apprentice*
                            "[DEL]"
                            `(delete-package
                              (find-package ',object))
                            :redisplay t)
      (princ " ")
      (put-lisp-button-here *apprentice*
                            "[CLREXT]"
                            `(loop :with pkg = (find-package ',object)
                                   :for sym :being
                                   :each :external-symbol
                                   :in pkg
                                   :do (unexport sym pkg))
                            :redisplay t)
      (princ " ")
      (put-lisp-button-here *apprentice*
                            "[CLRSYM]"
                            `(loop :with pkg = (find-package ',object)
                                   :for sym :being
                                   :each :symbol
                                   :in pkg
                                   :do (unintern sym pkg))
                            :redisplay t)
      (terpri)
      (terpri)
      (describe package)
      (format t "~%Export form:~2%")
      (format t "(EXPORT '(~{~A~^~%          ~}))"
              (sort (loop :for sym :being :each
                          :external-symbol :in package
                          :collect sym)
                    #'string<))
      t)))

(defun example-method-description (method)
  (format nil "~S (~{~S~^ ~})"
          (closer-mop:generic-function-name
           (closer-mop:method-generic-function method))
          (mapcar (lambda (spec)
                    (if (typep spec 'class)
                        (class-name spec)
                        (list 'eql
                              (closer-mop:eql-specializer-object spec))))
                  (closer-mop:method-specializers
                   method))))

(defun example-describe-class-maybe (object stream)
  (when (and (symbolp object)
             (not (eq t object))
             (find-class object nil))
    (let* ((*standard-output* stream)
           (class (find-class object))
           (methods (closer-mop:specializer-direct-methods
                     class))
           (descriptions
             (sort (mapcar 'example-method-description methods)
                   #'string<)))
      (when methods
        (format t "Methods related to the class ~A:~2%" object)
        (format t "~{~A~^~%~}" descriptions)
        t))))

(defmethod describe-with-apprentice ((ap example-apprentice)
                                     (c looking-at-character)
                                     stream)
  (setf (state ap) 'toplevel)
  (unwind-protect (call-next-method)
    (setf (state ap) nil)))

(let ((*caching-apprentice-default-update-interval* 1))
  (setf *apprentice*
        (make-instance 'example-apprentice
          :toplevel (list (make-instance 'suggest-apprentice)
                          (make-instance 'wide-toplevel-apprentice))
          :apprentices
          (list
           'suggest-apprentice
           'describe-apprentice
           #'example-describe-package-maybe
           'value-apprentice
           'grep-apprentice
           (make-instance 'apropos-apprentice
             :min-length 2
             :interesting-symbol-function
             (lambda (sym)
               ;; Ignore mixed case symbols.
               (if (eq (symbol-package sym) *package*)
                   (not (find-if #'lower-case-p (symbol-name sym)))
                   t)))
           #'example-describe-class-maybe))))

