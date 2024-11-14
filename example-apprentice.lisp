;;;; Requires
;;;;   apprentice
;;;;   closer-mop
;;;;   "apprentice-gathering"
;;;;   "apropos-apprentice"
;;;;   "grep-apprentice"
;;;;   "describe-apprentice"
;;;;   "value-apprentice"
;;;;   "method-apprentice"
;;;;   "toplevel-apprentice"
;;;;   "suggest-apprentice"
;;;;   "buttons"

(in-package :apprentice)

(defclass Example-apprentice (apprentice-gathering)
  ((toplevel-apprentices
    :initarg :toplevel-apprentices
    :accessor toplevel-apprentices
    :initform nil))
  (:documentation ""))

(defmethod initialize-instance :after ((e example-apprentice)
                                       &key toplevel-apprentices)
  (setf (toplevel-apprentices e)
        (mapcar 'instantiate-maybe toplevel-apprentices)))

(defmethod apprentices-based-on-input ((ap example-apprentice)
                                       (input looking-at-character))
  (toplevel-apprentices ap))

(defun example-apprentice-reload (object stream)
  (declare (ignore object))
  (princ "                         " stream)
  (put-lisp-button-here
   *apprentice*
   "[RELOAD]"
   '(asdf:load-system "apprentice/example-apprentice" :force t)
   :stream stream))

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
                            `(let ((pkg (find-package ',object))
                                   (ext '()))
                               (do-external-symbols (sym pkg)
                                 (push sym ext))
                               (unexport ext pkg))
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

(defun Make-example-apprentice ()
  (let* ((*caching-apprentice-default-update-interval* 2)
         (activity-apprentice (make-instance 'activity-apprentice
                                :history-length 5
                                :proximity-cutoff 60)))
    (make-instance 'example-apprentice
      :toplevel-apprentices
      (list 'suggest-apprentice
            activity-apprentice
            'wide-toplevel-apprentice
            #'example-apprentice-reload)
      :apprentices
      (list 'suggest-apprentice
            activity-apprentice
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
                    (not (find-if #'lower-case-p
                                  (symbol-name sym)))
                    t)))))))

(setf *apprentice* (make-example-apprentice))

