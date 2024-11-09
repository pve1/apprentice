;;;; Requires
;;;;   closer-mop
;;;;   "apprentice"
;;;;   "buttons"
;;;;   "temporary-apprentice"

(in-package :apprentice) cx

(defclass Describe-apprentice ()
  ())

(defun describe-toggle-variable (symbol)
  (let* ((nothing (load-time-value '#:nothing))
         (other-value (get symbol
                           'describe-apprentice-toggle-value
                           nothing)))
    (if (eq other-value nothing)
        (setf (symbol-value symbol)
              (not (symbol-value symbol)))
        (let ((current (symbol-value symbol)))
          (setf (symbol-value symbol) other-value
                (get symbol 'describe-apprentice-toggle-value)
                current)))
    t))

(defun describe-apprentice-has-toggle-p (symbol)
  (let* ((nothing (load-time-value '#:nothing))
         (other-value (get symbol
                           'describe-apprentice-toggle-value
                           nothing)))
    (not (eq other-value nothing))))

(defun describe-remove-class-direct-methods (class)
  (dolist (method (closer-mop:specializer-direct-methods
                   (find-class class)))
    (let ((gf (closer-mop:method-generic-function method)))
      (remove-method gf method))))

(defun describe-class (symbol stream)
  (when (and (symbolp symbol)
             (not (eq t symbol))
             (find-class symbol nil))
    (let* ((*standard-output* stream)
           (class (find-class symbol))
           (methods (closer-mop:specializer-direct-methods
                     class))
           (descriptions
             (mapcar #'method-apprentice-method-description methods))
           (formatted-descriptions
             (mapcar (lambda (x)
                       (method-apprentice-format-description x nil))
                     descriptions))
           (sorted (sort formatted-descriptions #'string<)))
      (when methods
        (format t "Related methods:~2%")
        (format t "~{~A~^~%~}" sorted)
        (terpri)))))

(defun describe-export-some-gfs (symbol)
  (let* ((gfs (closer-mop:specializer-direct-generic-functions
               (find-class symbol)))
         (symbol-package (symbol-package symbol))
         (to-export))
    (dolist (gf gfs)
      (alexandria:when-let*
          ((name (closer-mop:generic-function-name gf))
           (symbolp (symbolp name)) ;; Not setf
           (pkg (symbol-package name)))
        (when (eq pkg (symbol-package symbol))
          (push name to-export))))
    (export to-export symbol-package)
    (format *debug-io* "; Exported ~{~A~^, ~}.~%" to-export)
    to-export))

(defmethod describe-with-apprentice ((ap describe-apprentice)
                                     object
                                     stream)
  (let ((*standard-output* stream))
    (flet ((space ()
             (princ " ")))
      (describe object)
      (when (and (symbolp object)
                 (find-class object nil))
        (terpri)
        (describe-class object stream))
      ;; Buttons
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
                                :redisplay t)
          (when (or (member (symbol-value object) '(t nil))
                    (describe-apprentice-has-toggle-p object))
            (space)
            (put-lisp-button-here ap
                                  "[TOGGLE]"
                                  `(describe-toggle-variable ',object)
                                  :stream stream
                                  :redisplay t)))
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
        (when (find-class object nil)
          (space)
          ;; Export related gfs
          (put-lisp-button-here
           *apprentice* "[EXPGF]"
           `(emacs-message (format nil "Exported ~S symbols."
                                   (length
                                    (describe-export-some-gfs
                                     ',object))))
           :stream stream)
          (space)
          ;; Remove class
          (put-lisp-button-here
           ap
           "[RMCLASS]"
           `(progn
              (describe-remove-class-direct-methods ',object)
              (setf (find-class ',object) nil)
              (emacs-message "Removed class and associated methods."))
           :stream stream
           :redisplay t)
          (space)
          ;; Remove methods
          (put-lisp-button-here
           *apprentice* "[RMMTHDS]"
           `(describe-remove-class-direct-methods ',object)
           :redisplay t
           :stream stream))
        (terpri stream))
      t)))

