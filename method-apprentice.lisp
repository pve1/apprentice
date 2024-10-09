;;;; Requires
;;;;   "apprentice"
;;;;   "buttons"

(in-package :apprentice) cx

(defclass Method-apprentice ()
  ())

(defvar *method-apprentice-methods* nil)

(defun method-apprentice-remove-method (method)
  (let ((gf (closer-mop:method-generic-function method)))
    (remove-method gf method)
    t))

(defun method-apprentice-dim-current-line ()
  (swank:eval-in-emacs
   '(progn
     (with-current-buffer apprentice-buffer-name
       (add-text-properties (save-excursion
                             (beginning-of-line)
                             (point))
        (save-excursion
         (end-of-line)
         (point))
        '(face apprentice-dim-face
          font-lock-face apprentice-dim-face))))))

(defmethod describe-with-apprentice ((ap method-apprentice)
                                     (object symbol)
                                     stream)
  (when (and (fboundp object)
             (typep (fdefinition object) 'generic-function))
    (let* ((*standard-output* stream)
           (function (fdefinition object))
           (methods (closer-mop:generic-function-methods function))
           (descriptions (mapcar (lambda (m)
                                   (method-apprentice-method-description ap m))
                                 methods)))
      (when methods
        (format t "Methods of ~A:~2%" object)
        (setf *method-apprentice-methods*
              (loop :for m :in methods
                    :for i :from 0
                    :collect (list i m)))
        (loop :for method :in methods
              :for i :from 0
              :do (let ((readable (method-apprentice-method-description
                                   ap method)))
                    (destructuring-bind (&key specializers qualifiers)
                        readable
                      (if qualifiers
                          (format t "(~{~S~^ ~})~{ ~S~} "
                                  specializers
                                  qualifiers)
                          (format t "(~{~S~^ ~}) "
                                  specializers))
                      (put-lisp-button-here
                       ap
                       "[REMOVE]"
                       `(progn
                          (method-apprentice-remove-method
                           (second (assoc ,i *method-apprentice-methods*)))
                          (setf *method-apprentice-methods* nil)
                          (method-apprentice-dim-current-line)))
                      (terpri))))
        t))))

(defmethod method-apprentice-method-description ((ap method-apprentice)
                                                 method)
  (list :specializers
        (mapcar (lambda (spec)
                  (if (typep spec 'class)
                      (class-name spec)
                      (list 'eql
                            (closer-mop:eql-specializer-object spec))))
                (closer-mop:method-specializers method))
        :qualifiers (closer-common-lisp:method-qualifiers method)))

