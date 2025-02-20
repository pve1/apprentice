;;;; Requires
;;;;   cl-interpol
;;;;   apprentice
;;;;   "buttons"
;;;;   "emacs"

(cl-interpol:enable-interpol-syntax)
(in-package :apprentice) cx

(defvar *debug-suggestions* nil)

(defclass Suggest-apprentice ()
  ((separatorp :initarg :separatorp
               :accessor separatorp-sg
               :initform nil)))

(defclass Suggestion ()
  ((string :initarg :string
           :accessor Suggestion-string
           :initform nil)
   (label :initarg :label
          :accessor Suggestion-label
          :initform nil)
   ;; Elisp form to evaluate before inserting suggestion.
   (pre-insert-elisp-form
    :initarg :pre-insert-elisp-form
    :accessor Pre-insert-elisp-form
    :initform nil)
   ;; Elisp form to evaluate after inserting suggestion.
   (post-insert-elisp-form
    :initarg :post-insert-elisp-form
    :accessor Post-insert-elisp-form
    :initform nil))
  (:documentation ""))

(defmethod suggestion-string ((s string))
  s)

(defmethod suggestion-label (object)
  nil)

(defmethod Apprentice-create-ephemerals (apprentice)
  (create-ephemeral-elisp-function
   apprentice 'insert-toplevel-suggestion
   '(lambda (buffer
             suggestion-string
             pre-insert-form
             post-insert-form)
     (switch-to-buffer-other-window buffer)
     (if pre-insert-form
         (eval (car (read-from-string
                     pre-insert-form)))
         (ignore-errors
          (progn
           (beginning-of-thing 'sexp)
           (kill-sexp))))
     (let ((begin (point)))
       (when suggestion-string
         (insert suggestion-string))
       (let ((end (point)))
         (when post-insert-form
           (eval (car (read-from-string
                       post-insert-form))))
         (indent-region begin end))))))

(defmethod describe-with-apprentice ((ap suggest-apprentice)
                                     object
                                     stream)
  (let* ((path+pos (suggest-get-current-path+pos ap))
         (suggestions (generate-suggestions ap object path+pos))
         (begin (file-position stream))
         (*standard-output* stream))
    (when suggestions
      (apprentice-create-ephemerals ap)
      (format t "Suggestions for ~A:~%" (if (car path+pos)
                                            path+pos
                                            "toplevel"))
      (terpri)
      (when *debug-suggestions*
        (format *debug-io* "~&################################~%"))
      (let ((mini-separators (length suggestions)))
        (dolist (suggestion suggestions)
          (when *debug-suggestions*
            (format *debug-io* "~&~A~%--------------------------------"
                    (suggestion-string suggestion)))
          (put-elisp-button-here
           ap (or (suggestion-label suggestion)
                  (suggestion-string suggestion))
           nil
           :name 'insert-toplevel-suggestion
           :arguments (list (buffer-context-property :buffer-name)
                            (suggestion-string suggestion)
                            (etypecase suggestion
                              (string nil)
                              (suggestion
                               (alexandria:when-let
                                   ((pre (pre-insert-elisp-form
                                          suggestion)))
                                 (process-form-for-emacs pre))))
                            (etypecase suggestion
                              (string nil)
                              (suggestion
                               (alexandria:when-let
                                   ((post (post-insert-elisp-form
                                           suggestion)))
                                 (process-form-for-emacs post)))))
           :face (when (suggestion-string suggestion)
                   :unspecified))
          (unless (alexandria:ends-with (suggestion-string suggestion)
                                        #\newline)
            (terpri))
          (unless (< (decf mini-separators) 1)
            (when (separatorp-sg ap)
              (format t "- - - - - - - - - - - - - - - -~%")))))
      (push-description-property
       `(indent-region ,begin ,(file-position stream))
       :last)
      t)))

;;; Helpers
(defmethod Suggest-get-class-accessors (ap (class-name symbol)
                                        &key (package (symbol-package
                                                       class-name)))
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

(defmethod Suggest-with-accessors-spec (ap class-name)
  (let ((accessors (suggest-get-class-accessors ap class-name))
        (name (string-downcase class-name)))
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
     accessors)))

(defmethod Suggest-get-class-slots (ap (class-name symbol))
  (let ((class (find-class class-name nil)))
    (when class
      (mapcar #'closer-mop:slot-definition-name
              (closer-mop:class-direct-slots class)))))

(defmethod Suggest-get-current-path (ap)
  (alexandria:when-let ((buffer (buffer-context-property :buffer-name)))
    (eval-in-emacs
     `(with-current-buffer ,buffer
        (apprentice-current-form-path)))))

(defmethod Suggest-get-current-path+pos (ap)
  (alexandria:when-let ((buffer (buffer-context-property :buffer-name)))
    (eval-in-emacs
     `(with-current-buffer ,buffer
        (apprentice-current-form-path+pos)))))

(defmethod Suggest-get-toplevel-name (ap)
  (alexandria:when-let ((buffer (buffer-context-property :buffer-name)))
    (eval-in-emacs
     `(with-current-buffer ,buffer
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

(defun format-suggestion-chomp-trim-right (string)
  (normalize-indentation
   (string-right-trim '(#\space)
                      (chomp-left-right string))))

(defun format-suggestion (string)
  (format-suggestion-chomp-trim-right string))

(defun map-tree-sg (predicate fn tree)
  (cond ((funcall predicate tree)
         (funcall fn tree))
        ((atom tree)
         tree)
        (t (cons (map-tree-sg predicate fn (car tree))
                 (if (cdr tree)
                     (map-tree-sg predicate fn (cdr tree))
                     nil)))))

(defgeneric Generate-suggestions (ap object path+pos)
  (:method (ap object path)
    nil)
  (:documentation ""))

;; Hardcode suggestions for now.
(defmethod generate-suggestions (ap (object symbol) path+pos)
  (let* ((suggestions ())
         (path (first path+pos))
         (position (second path+pos))
         (name (string-downcase (symbol-name object)))
         (name-looks-like-special-variable
           (and (alexandria:starts-with #\* name)
                (alexandria:ends-with #\* name)))
         (line (buffer-context-property :line))
         (toplevel-name)
         (up-list-kill-form
           `(progn
              (up-list -1)
              (kill-sexp))))
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
      (when (and (null path)
                 (not (keywordp object)))
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
        ;; Eval-when
        (when (and (null path)
                   (eq 'eval-when object))
          (suggest #?{(eval-when (:compile-toplevel :load-toplevel :execute)
                        )}))
        ;; Defmethod for existing gf
        (when (and (fboundp object)
                   (typep (fdefinition object) 'generic-function))
          (let ((name (string-downcase object))
                (lambda-list
                  (map-tree-sg #'symbolp
                               #'string-downcase
                               (closer-mop:generic-function-lambda-list
                                (fdefinition object))))
                (qualifiers (if (eq 'initialize-instance object)
                                " :after"
                                "")))
            (suggest #?{
                     (defmethod $(name)$(qualifiers) $(lambda-list)
                       )})))
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
            (suggest #?{(defun ${name} ${(or slot-strings "()")}
                          (make-instance '${name}
                            @{kw-slot-pairs}))
                     }))
          ;; With-foo
          (let* ((cl-interpol:*list-delimiter* #?"\n")
                 (accessor-spec (suggest-with-accessors-spec ap object)))
            (suggest #?{
                     (defmacro with-${name} (instance &body body)
                       `(with-accessors (@{accessor-spec})
                            ,instance
                          ,@body))
                     })))
        ;; Toplevel definitions (not fboundp)
        (when (and (not (fboundp object))
                   (not name-looks-like-special-variable))
          (let ()
            ;; Defun
            (suggest #?{
                     (defun $(name) ()
                       )})
            ;; Defgeneric
            (suggest #?{
                     (defgeneric $(name) ()
                       (:documentation ""))})
            ;; Defmethod
            (suggest #?{
                     (defmethod $(name) ()
                       )})))
        ;; Defvar and defparameter
        (when name-looks-like-special-variable
          (suggest #?{
                   (defvar $(name))})
          (suggest #?{
                   (defparameter $(name) nil)}))
        ;; Defclass
        (unless (or (find-class object nil)
                    name-looks-like-special-variable)
          (suggest #?{
                   (defclass ${name} ()
                     ()
                     (:documentation ""))})))
      ;; Not toplevel
      ;; Exports
      (when (path-starts-with '("defpackage" ":export"))
        (let ((cl-interpol:*list-delimiter* #?"\n#:")
              exports)
          (do-external-symbols (sym *package*)
            (push (string-downcase sym) exports))
          (setf exports (sort exports #'string<))
          (suggest (if exports
                       #?{(:export #:@{exports})}
                       #?{(:export)})
                   :pre-insert-elisp-form up-list-kill-form)))
      ;; Imports
      (when (path-starts-with '("defpackage" ":import-from"))
        (let ((cl-interpol:*list-delimiter* #?"\n")
              (imports (make-hash-table)))
          (loop :for sym :being :each
                :present-symbol :in *package*
                :when (not (eq *package* (symbol-package sym)))
                :do (push sym (gethash (symbol-package sym) imports)))
          (let* ((packages (alexandria:hash-table-keys imports))
                 (clauses
                   (mapcar
                    (lambda (p)
                      (let ((cl-interpol:*list-delimiter* #?"\n#:")
                            (symbols (sort
                                      (mapcar #'string-downcase
                                              (gethash p imports))
                                      #'string<)))
                        #?{(:import-from #:${(string-downcase
                                              (package-name p))}
                                              #:@{symbols})}))
                    packages)))
            (suggest #?{@{clauses}}
                     :pre-insert-elisp-form up-list-kill-form))))
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
        (let* ((forms (suggest-with-accessors-spec ap object))
               (cl-interpol:*list-delimiter* #?"\n"))
          (suggest #?{(@{forms})})))
      ;; With-slots
      (when (and (find-class object nil)
                 (path-ends-with '("with-slots")))
        (let* ((slots (suggest-get-class-slots ap object))
               (forms (mapcar #'string-downcase slots)))
          (suggest #?{(@{forms})})))
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
                (mapcar (lambda (x)
                          (let ((param (if (consp x)
                                           (car x)
                                           x)))
                            (if (symbolp param)
                                (string-downcase param)
                                param)))
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
      (when (and (or (path-ends-with '("defmethod"))
                     (path-ends-with '("defun")))
                 (<= 3 position)) ; After lambda-list
        (let ((downcased (string-downcase object)))
          ;; Declare ignore
          (suggest #?{(declare (ignore ${downcased}))})
          ;; Declare type
          (if (find-class object nil)
              (suggest #?{(declare (type ${downcased} _))}
                       :post-insert-elisp-form `(search-backward "_"))
              (suggest #?{(declare (type _ ${downcased}))}
                       :post-insert-elisp-form `(search-backward "_")))
          ;; Check-type
          (if (find-class object nil)
              (suggest #?{(check-type _ ${downcased})}
                       :post-insert-elisp-form `(search-backward "_"))
              (suggest #?{(check-type ${downcased} _)}
                       :post-insert-elisp-form `(search-backward "_"))))
        (alexandria:when-let*
            ((class (find-class object nil))
             (type (typep class 'standard-class))
             (accessors (suggest-with-accessors-spec ap object))
             (cl-interpol:*list-delimiter* #?"\n"))
          (suggest #?{
                   (with-accessors (@(accessors))
                     )})))
      ;; Loop
      (when (and (path-ends-with '("loop"))
                 (eq 'loop object))
        (let ((post-form `(progn
                            (save-excursion
                             (backward-sexp)
                             (when (looking-back "(")
                               (backward-delete-char 1))))))
          (suggest #?{(loop :for key :being :the
                            :hash-key :of hashtable
                            :using (:hash-value value))}
                            :post-insert-elisp-form post-form)
          (suggest #?{(loop :for value :being :the
                            :hash-value :of hashtable
                            :using (:hash-key key))}
                            :post-insert-elisp-form post-form)
          (suggest #?{(loop :for symbol :being
                            :each :symbol
                            ;; :each :present-symbol
                            ;; :each :external-symbol
                            :of package)})
                            :post-insert-elisp-form post-form))
      (nreverse suggestions))))

;; Quick 'n' dirty copy-paste from symbol method.
(defmethod generate-suggestions (ap (object looking-at-character)
                                 path+pos)
  (let ((suggestions ())
        (path (first path+pos))
        (position (second path+pos))
        (line (buffer-context-property :line))
        (file (buffer-context-property :filename))
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
      ;; Monitor form
      (when (buffer-context-property :region)
        (push (make-instance 'suggestion
                :label "[MONITOR FORM]"
                :pre-insert-elisp-form '(progn)
                :post-insert-elisp-form
                '(progn
                  (goto-char (region-end))
                  (apprentice-describe-form)))
              suggestions))
      ;; Beginning of asd file
      (when (and file
                 (equal "asd" (pathname-type file))
                 (null (preceding-char object)))
        (let ((system-name (pathname-name file)))
          (when system-name
            (suggest #?{
                     (asdf:defsystem #:${system-name}
                       :description ""
                       :author ""
                       :license ""
                       :version "0.0.1"
                       :serial t
                       :components ((:file "${system-name}"))
                       :depends-on ())
                     })
            (suggest #?{
                     (asdf:defsystem #:${system-name}
                       :description ""
                       :author ""
                       :license ""
                       :version "0.0.1"
                       :class :package-inferred-system
                       :depends-on ("${system-name}/${system-name}"))
                     })
            (suggest #?{
                     (asdf:defsystem #:${system-name}
                       :description ""
                       :author ""
                       :license ""
                       :version "0.0.1"
                       :class "extensible-inferred-system:comment-system"
                       :defsystem-depends-on (:extensible-inferred-system)
                       :depends-on ("${system-name}/${system-name}"))
                     }))))
      ;; Empty file
      (when (and file
                 (null (preceding-char object))
                 (null (following-char object)))
        ;; Package-inferred-system defpackage
        (let ((search-max-tries 3))
          ;; Note: DIRECTORY returns truenames, so this might break if
          ;; symlinks are present.
          (labels ((up-dir (path)
                     (let* ((dir (pathname-directory path))
                            (len (length dir)))
                       (when (and dir (< 1 len))
                         (make-pathname
                          :directory (butlast (pathname-directory path))))))
                   (search-asd (path &optional (tries search-max-tries) acc)
                     (when (and (<= 1 tries) path)
                       (alexandria:when-let*
                           ((asds (directory
                                   (merge-pathnames "*.asd" path)))
                            (ranked (sort asds
                                          (lambda (x y)
                                            (< (length (pathname-name x))
                                               (length (pathname-name y))))))
                            (first (first ranked)))
                         (return-from search-asd
                           (cons (pathname-name first) acc)))
                       (search-asd (up-dir path)
                                   (1- tries)
                                   (cons (alexandria:last-elt
                                          (pathname-directory path))
                                         acc)))))
            (let* ((asd-name (search-asd file))
                   (package-inferred-package-name
                     (format nil "~{~A~^/~}/~A"
                             asd-name
                             (pathname-name file))))
                   (when asd-name
                     (suggest #?{
                              (defpackage #:${package-inferred-package-name}
                                (:use #:cl)
                                ;; (:local-nicknames)
                                ;; (:import-from)
                                (:export))

                              ${""}(in-package #:${package-inferred-package-name})
                              }))))))
      ;; Accessors
      (when (and (equal path '("defclass"))
                 (eql #\) (preceding-char object))
                 (= position 3))
        (alexandria:when-let*
            ((preceding-list
              (eval-in-emacs
               `(with-current-buffer
                    ,(buffer-context-property :buffer-name)
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
          (suggest #?{(@{slot-definitions})})))
      (nreverse suggestions))))
