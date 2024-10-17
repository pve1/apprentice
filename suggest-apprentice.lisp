;;;; Requires
;;;;   cl-ppcre
;;;;   cl-interpol
;;;;   "apprentice"
;;;;   "buttons"

(cl-interpol:enable-interpol-syntax)
(in-package :apprentice) cx

(defclass Suggest-apprentice ()
  ())

(defclass suggestion ()
  ((string :initarg :string
           :accessor suggestion-string
           :initform nil)
   (pre-insert-elisp-form
    :initarg :pre-insert-elisp-form
    :accessor pre-insert-elisp-form
    :initform nil)
   (post-insert-elisp-form
    :initarg :post-insert-elisp-form
    :accessor post-insert-elisp-form
    :initform nil))
  (:documentation ""))

(defmethod suggestion-string ((s string))
  s)

(defmethod suggest-prepare-elisp-functions (apprentice)
  (create-ephemeral-elisp-function
   apprentice 'insert-toplevel-suggestion
   '(lambda (file
             suggestion-string
             pre-insert-form
             post-insert-form)
     (switch-to-buffer-other-window
      (get-file-buffer file))
     (if pre-insert-form
         (eval (car (read-from-string
                     pre-insert-form)))
         (ignore-errors
          (progn
           (beginning-of-thing 'sexp)
           (kill-sexp))))
     (insert suggestion-string)
     (when post-insert-form
       (eval (car (read-from-string
                   post-insert-form))))
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
         ap (suggestion-string suggestion) nil
         :name 'insert-toplevel-suggestion
         :arguments (list (getf *buffer-context* :filename)
                          (suggestion-string suggestion)
                          (etypecase suggestion
                            (string nil)
                            (suggestion
                             (alexandria:when-let
                                 ((pre (pre-insert-elisp-form
                                        suggestion)))
                               (swank::process-form-for-emacs pre))))
                          (etypecase suggestion
                            (string nil)
                            (suggestion
                             (alexandria:when-let
                                 ((post (post-insert-elisp-form
                                         suggestion)))
                               (swank::process-form-for-emacs post)))))
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
  (let ((indentation nil)
        (lines 0))
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
                    :for i :from 0
                    :until (eq s line)
                    :minimize (measure-indentation line)
                    :finally (setf lines i)))
        (file-position s 0)
        (with-output-to-string (out)
          (loop :with missing-newline
                :for line = (multiple-value-bind (line missing)
                                (read-line s nil s)
                              (setf missing-newline missing)
                              line)
                :for i :from 0
                :until (eq s line)
                :do (unless (empty-line-p line)
                      (write-sequence
                       line out :start indentation))
                    (unless (and (eq s (peek-char nil s nil s))
                                 missing-newline)
                      (terpri out))))))))

(defun chomp-left (string)
  (if (and (not (alexandria:emptyp string))
           (eql #\Newline (alexandria:first-elt string)))
      (subseq string 1)
      string))

(defun chomp-right (string)
  (if (and (not (alexandria:emptyp string))
           (eql #\Newline (alexandria:last-elt string)))
      (subseq string 0 (1- (length string)))
      string))

(defun chomp-left-right (string)
  (chomp-right (chomp-left string)))

(defun format-suggestion-trim-chomp (string)
  (normalize-indentation
   (chomp-left-right
    (string-trim '(#\space) string))))

(defun format-suggestion-chomp-trim (string)
  (normalize-indentation
   (string-trim '(#\space)
                (chomp-left-right string))))

(defun format-suggestion (string)
  (format-suggestion-chomp-trim string))

(defgeneric generate-suggestions (ap object path)
  (:method (ap object path)
    nil)
  (:documentation ""))

;; Hardcode suggestions for now.
(defmethod generate-suggestions (ap (object symbol) path)
  (let ((suggestions ())
        (name (string-downcase (symbol-name object)))
        (line (getf *buffer-context* :line))
        (toplevel-name))
    (flet ((suggest (x &key pre-insert-elisp-form
                            post-insert-elisp-form)
             (push (if (or pre-insert-elisp-form
                           post-insert-elisp-form)
                       (make-instance 'suggestion
                         :string (format-suggestion x)
                         :pre-insert-elisp-form pre-insert-elisp-form
                         :post-insert-elisp-form post-insert-elisp-form)
                       (format-suggestion x))
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
          (suggest #?(
                      (defpackage #:${name}
                        (:use #:cl)
                        (:local-nicknames)
                        ;; (:import-from)
                        (:export))

                      ${""}(in-package #:${name})
                      ))
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
                          :initform nil)})
        (suggest #?{
                 (${name} :initarg :${name}
                          :accessor ${(toplevel-name)}-${name}
                          :initform nil)})
        (suggest #?{
                 (${name} :initarg :${name}
                          :accessor ${name}-of
                          :initform nil)}))
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
                   (@{forms})})))
      ;; With-slots
      (when (and (find-class object nil)
                 (path-ends-with '("with-slots")))
        (let* ((slots (suggest-get-class-slots ap object))
               (forms (mapcar #'string-downcase slots)))
          (suggest #?{
                   (@{forms})})))
      ;; Suggest
      (when (path-ends-with '("suggest"))
        (suggest #?|
                 suggest #?{

                          })
                 |))
      ;; Defgeneric from defmethod, point at function name
      (when (and (equal '("defmethod") path)
                 (fboundp object)
                 (typep (fdefinition object)
                        'generic-function))
        (let ((lambda-list
                (mapcar #'string-downcase
                        (closer-mop:generic-function-lambda-list
                         (fdefinition object)))))
          (suggest #?{
                   (defgeneric ${name} ${lambda-list}
                     (:documentation ""))

                   }
                   :pre-insert-elisp-form
                   `(progn
                      (apprentice-goto-toplevel)
                      (beginning-of-line)))))
      (nreverse suggestions))))

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
                   (@{slot-definitions})})))
      (nreverse suggestions))))
