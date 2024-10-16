;;;; Requires
;;;;   cl-ppcre
;;;;   cl-interpol
;;;;   "apprentice"
;;;;   "buttons"

(cl-interpol:enable-interpol-syntax)
(in-package :apprentice) cx

(defclass Suggest-apprentice (caching-apprentice)
  ((busy-result :initarg :busy-result
                :accessor busy-result
                :initform nil)
   (suggestion-table :initarg :suggestion-table
                     :accessor suggestion-table
                     :initform (make-hash-table :test 'equal))))

(defmethod suggest-prepare-elisp-functions (apprentice)
  (create-ephemeral-elisp-function
   apprentice 'insert-toplevel-suggestion
   '(lambda (file suggestion-string)
     (switch-to-buffer-other-window
      (get-file-buffer file))
     (ignore-errors
      (progn
        (beginning-of-thing 'sexp)
        (kill-sexp)))
     (insert suggestion-string)
     (save-excursion
      (backward-sexp)
      (indent-sexp)))))

(defmethod describe-with-apprentice ((ap suggest-apprentice)
                                     object
                                     stream)
  (let* ((path (suggest-get-current-path ap))
         (suggestions (generate-suggestions ap object path))
         (begin (file-position stream))
         (*standard-output* stream))
    (when suggestions
      (suggest-prepare-elisp-functions ap)
      (format t "Suggestions for ~A:~%" (if path
                                            path
                                            "toplevel"))
      (terpri)
      (dolist (suggestion suggestions)
        (put-elisp-button-here
         ap suggestion nil
         :name 'insert-toplevel-suggestion
         :arguments (list (getf *buffer-context* :filename)
                          suggestion)
         :face :unspecified)
        (terpri)
        (terpri))
      (alexandria:appendf
       *description-properties*
       `((indent-region ,begin ,(+ begin (file-position stream))))))))

;;; Helpers
(defmethod suggest-get-class-accessors (ap (class-name symbol)
                                        &key (package *package*))
  (let ((class (find-class class-name nil))
        (generic-functions))
    (when class
      (loop :with specializer = (list class)
            :for sym :being :each :present-symbol
            :in package
            :do (alexandria:when-let*
                    ((bound (fboundp sym))
                     (fn (fdefinition sym))
                     (is-gf (typep fn 'generic-function))
                     (lambda-list
                      (closer-mop:generic-function-lambda-list fn))
                     (is-accessor (= 1 (length lambda-list)))
                     (method (find-method fn nil specializer nil)))
                  ;; Found accessor.
                  (push sym generic-functions)))
      generic-functions)))

(defmethod suggest-get-class-slots (ap (class-name symbol))
  (let ((class (find-class class-name nil)))
    (when class
      (mapcar #'closer-mop:slot-definition-name
              (closer-mop:class-direct-slots class)))))

(defmethod suggest-get-current-path (ap)
  (alexandria:when-let ((file (getf *buffer-context* :filename)))
    (swank:eval-in-emacs
     `(with-current-buffer (get-file-buffer ,file)
        (apprentice-current-form-path)))))

(defmethod suggest-get-toplevel-name (ap)
  (alexandria:when-let ((file (getf *buffer-context* :filename)))
    (swank:eval-in-emacs
     `(with-current-buffer (get-file-buffer ,file)
        (apprentice-toplevel-form-name)))))

;; Hack
(defun normalize-indentation (string)
  (let ((indentation nil))
    (labels ((empty-line-p (line)
               (loop :for c :across line
                     :always (eql c #\Space)))
             (measure-indentation (line)
               (if (empty-line-p line)
                   999999
                   (loop :for c :across line
                         :for i :from 0
                         :until (not (eql c #\Space))
                         :finally (return i)))))
      (with-input-from-string (s string)
        (setf indentation
              (loop :for line = (read-line s nil s)
                    :until (eq s line)
                    :minimize (measure-indentation line)))
        (file-position s 0)
        (with-output-to-string (out)
          (loop :for line = (read-line s nil s)
                :until (eq s line)
                :do (write-sequence line out
                                    :start (if (empty-line-p line)
                                               0
                                               indentation))
                    (terpri out)))))))

(defun format-suggestion (string)
  (string-trim '(#\newline #\space)
               (normalize-indentation string)))

;; Hardcode suggestions for now.
(defmethod generate-suggestions (ap (object symbol) path)
  (let ((suggestions ())
        (name (string-downcase (symbol-name object)))
        (line (getf *buffer-context* :line))
        (toplevel-name))
    (flet ((suggest (x)
             (push (format-suggestion x)
                   suggestions))
           (toplevel-name ()
             (or toplevel-name
                 (setf toplevel-name
                       (suggest-get-toplevel-name ap))))
           (path-starts-with (prefix)
             (alexandria:starts-with-subseq
              (alexandria:ensure-list prefix)
              path :test #'equal))
           (path-ends-with (suffix)
             (alexandria:ends-with-subseq
              (alexandria:ensure-list suffix)
              path :test #'equal)))
      ;; Toplevel
      (when (null path)
        ;; Defpackage
        (when (and line (<= line 3))
          ;; This form and the next messes with slime's package
          ;; detection unless the ${""} is added before the
          ;; in-package.
          (suggest #?{
                   (defpackage #:${name}
                     (:use #:cl)
                     (:local-nicknames)
                     ;; (:import-from)
                     (:export))

                   ${""}(in-package #:${name})
                   })
          ;; Make package
          (suggest #?{
                   (eval-when (:compile-toplevel :load-toplevel :execute)
                     (unless (find-package '#:${name})
                       (make-package '#:${name} :use '(#:cl))))

                   ${""}(in-package #:${name})

                   (export '())
                   }))
        ;; Defclass
        (unless (find-class object nil)
          (suggest #?{
                   (defclass ${name} ()
                     ()
                     (:documentation ""))
                   }))
        (when (find-class object nil)
          ;; initialize-instance
          (suggest #?{
                   (defmethod initialize-instance :after ((${(subseq name 0 1)} ${name}) &key)
                     )
                   })
          ;; print-object
          (let ((short (subseq name 0 1)))
            (suggest #?{
                     (defmethod print-object ((${short} ${name}) stream)
                       (print-unreadable-object (${short} stream :type t)
                         (format stream ""))
                       ${short})
                     }))
          ;; Boa constructor
          (let* ((cl-interpol:*list-delimiter* #?"\n")
                 (slots (suggest-get-class-slots ap object))
                 (slot-strings (mapcar (lambda (x)
                                         (string-downcase (symbol-name x)))
                                       slots))
                 (kw-slot-pairs
                   (loop :for slot :in slot-strings
                         :collect (format nil ":~A ~A"
                                          slot slot))))
            (suggest #?{
                     (defun ${name} ${slot-strings}
                       (make-instance '${name}
                         @{kw-slot-pairs}))
                     }))))
      ;; Accessors
      (when (and (<= 2 (length path))
                 (path-starts-with '("defclass")))
        (suggest #?{
                 (${name} :initarg :${name}
                          :accessor ${name}
                          :initform nil)
                 })
        (suggest #?{
                 (${name} :initarg :${name}
                          :accessor ${(toplevel-name)}-${name}
                          :initform nil)
                 })
        (suggest #?{
                 (${name} :initarg :${name}
                          :accessor ${name}-of
                          :initform nil)
                 }))
      ;; With-accessors
      (when (and (find-class object nil)
                 (path-ends-with '("with-accessors")))
        (let* ((accessors (suggest-get-class-accessors ap object))
               (forms
                 (mapcar
                  (lambda (x)
                    (let* ((acc (string-downcase x))
                           (short (if (and (alexandria:starts-with-subseq
                                            name acc)
                                           (< (1+ (length name))
                                              (length acc)))
                                      (subseq acc (1+ (length name)))
                                      acc)))
                      (format nil "(~A ~A)"
                              short
                              acc)))
                  accessors))
               (cl-interpol:*list-delimiter* #?"\n"))
          (suggest #?{
                   (@{forms})
                   })))
      ;; With-slots
      (when (and (find-class object nil)
                 (path-ends-with '("with-slots")))
        (let* ((slots (suggest-get-class-slots ap object))
               (forms (mapcar #'string-downcase slots)))
          (suggest #?{
                   (@{forms})
                   })))
      ;; Suggest
      (when (path-ends-with '("suggest"))
        (suggest #?|
                 suggest #?{

                          })
                 |))
      (nreverse suggestions))))

(defmethod generate-suggestions (ap (object looking-at-character)
                                 path)
  nil)

;; Quick 'n' dirty copy-paste from symbol method.
(defmethod generate-suggestions (ap (object looking-at-character)
                                 path)
  (let ((suggestions ())
        (line (getf *buffer-context* :line))
        (toplevel-name))
    (flet ((suggest (x)
             (push (format-suggestion x)
                   suggestions))
           (toplevel-name ()
             (or toplevel-name
                 (setf toplevel-name
                       (suggest-get-toplevel-name ap))))
           (path-starts-with (prefix)
             (alexandria:starts-with-subseq
              prefix path :test #'equal))
           (path-ends-with (suffix)
             (alexandria:ends-with-subseq
              suffix path :test #'equal)))
      ;; Accessors
      (when (and (equal path '("defclass"))
                 (eql #\) (preceding-char object)))
        (alexandria:when-let*
            ((preceding-list
              (swank:eval-in-emacs
               `(with-current-buffer
                    (get-file-buffer
                     ,(getf *buffer-context* :filename))
                  (save-excursion
                   (let ((end (point)))
                     (backward-sexp)
                     (let ((begin (point)))
                       (buffer-substring-no-properties
                        begin end)))))))
             (slots (read-from-string preceding-list))
             (slot-definitions
              (when (every #'symbolp slots)
                (loop :for slot :in slots
                      :for string = (string-downcase slot)
                      :collect
                         #?{(${string} :initarg :${string}
                                       :accessor ${string}
                                       :initform nil)})))
             (cl-interpol:*list-delimiter* #?"\n"))
          (suggest #?{
                   (@{slot-definitions})
                   })))
      (nreverse suggestions))))
