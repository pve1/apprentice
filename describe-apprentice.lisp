;;;; Requires
;;;;   closer-mop
;;;;   apprentice
;;;;   "buttons"
;;;;   "temporary-apprentice"
;;;;   "emacs"

(in-package :apprentice) cx

(defclass Describe-apprentice ()
  ())

(defun describe-toggle-package-qualifier (apprentice symbol buffer)
  (alexandria:when-let* ((pkg (symbol-package symbol))
                         (pred (lambda (sym)
                                 (eq symbol sym)))
                         (edits (compute-edits-for-toggle-qualifier
                                 (emacs-buffer-string buffer)
                                 pkg
                                 :symbol-predicate pred)))
    (emacs-perform-edits buffer edits)))

(defun describe-apprentice-has-toggle-p (symbol)
  (let* ((nothing (load-time-value '#:nothing))
         (other-value (get symbol
                           'describe-apprentice-toggle-value
                           nothing)))
    (not (eq other-value nothing))))

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
    (format *debug-io* "~&; Exported ~{~A~^, ~}.~%" to-export)
    to-export))

(defmethod describe-with-apprentice ((ap describe-apprentice)
                                     object
                                     stream)
  (let ((*standard-output* stream))
    (labels ((space ()
               (princ " "))
             (put-buttons-here ()
               (when (symbolp object)
                 ;; Unintern, shadow
                 (multiple-value-bind (sym state)
                     (find-symbol (symbol-name object) *package*)
                   (cond ((and (eq sym object)
                               (not (eq :inherited state)))
                          (put-lisp-button-here
                           ap
                           "[UNINTERN]"
                           `(unintern ',object
                                      (find-package
                                       ,(package-name *package*)))
                           :stream stream
                           :redisplay t)
                          (space))
                         ((and (eq sym object)
                               (eq :inherited state))
                          (put-lisp-button-here
                           ap
                           "[SHADOW]"
                           `(shadow ',object (find-package
                                              ,(package-name *package*)))
                           :stream stream)
                          (space))))
                 ;; Export
                 (multiple-value-bind (sym state)
                     (find-symbol (symbol-name object) *package*)
                   (cond ((and (eq object sym)
                               (eq state :external))
                          (put-lisp-button-here
                           ap
                           "[UNEXPORT]"
                           `(unexport ',object
                                      (find-package
                                       ,(package-name *package*)))
                           :stream stream
                           :redisplay t)
                          (space))
                         ((eq object sym)
                          (put-lisp-button-here
                           ap
                           "[EXPORT]"
                           `(export ',object (find-package
                                              ,(package-name *package*)))
                           :stream stream
                           :redisplay t)
                          (space))
                         ((and (null state)
                               (symbol-package object))
                          (put-lisp-button-here
                           ap
                           "[IMPORT]"
                           `(import ',object (find-package
                                              ,(package-name *package*)))
                           :stream stream
                           :redisplay t)
                          (space))
                         ((and state
                               (not (eq sym object))
                               (symbol-package object))
                          (put-lisp-button-here
                           ap
                           "[SHDWIMPORT]"
                           `(shadowing-import
                             ',object (find-package ,(package-name
                                                      *package*)))
                           :stream stream
                           :redisplay t)
                          (space))))
                 ;; Qualifier
                 (when (and (symbolp object)
                            (symbol-package object)
                            (not (keywordp object)))
                   (put-lisp-button-here
                    ap
                    "[QUAL]"
                    `(describe-toggle-package-qualifier
                      *button-apprentice*
                      ',object
                      ,(buffer-context-property :buffer-name))
                    :stream stream)
                   (space))
                 ;; Makunbound
                 (when (and (boundp object)
                            (not (keywordp object)))
                   (put-lisp-button-here ap
                                         "[MAKUNBOUND]"
                                         `(makunbound ',object)
                                         :stream stream
                                         :redisplay t)
                   (space)
                   (when (or (member (symbol-value object) '(t nil))
                             (describe-apprentice-has-toggle-p object))
                     (put-lisp-button-here ap
                                           "[TOGGLE]"
                                           `(describe-toggle-variable
                                             ',object)
                                           :stream stream
                                           :redisplay t)
                     (space)))
                 ;; Fmakunbound
                 (when (fboundp object)
                   (put-lisp-button-here ap
                                         "[FMAKUNBOUND]"
                                         `(fmakunbound ',object)
                                         :stream stream
                                         :redisplay t)
                   (space))
                 ;; Methods
                 (when (and (fboundp object)
                            (typep (fdefinition object) 'generic-function)
                            (closer-mop:generic-function-methods
                             (fdefinition object)))
                   (put-lisp-button-here
                    ap
                    "[METHODS]"
                    `(set-temporary-apprentice
                      'method-apprentice
                      (lambda (ap obj str)
                        (terpri str)
                        (describe-with-apprentice ap
                                                  obj
                                                  str)))
                    :stream stream
                    :redisplay t)
                   (space))
                 (when (find-class object nil)
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
                 (terpri stream))))
      (format stream "Description: ")
      (put-buttons-here)
      (terpri stream)
      (describe object)
      (when (and (symbolp object)
                 (find-class object nil))
        (terpri)
        (describe-class object stream))
      t)))

