;;;; Requires
;;;;   apprentice
;;;;   eclector
;;;;   "buttons"
;;;;   "emacs"

;;;; System apprentice

;;;; This apprentice will try to figure out to which system the
;;;; current buffer belongs and then load it (using asdf:load).

;;;; Reloading means restarting lisp and then loading the system.

(in-package :apprentice) cx

(defclass System-apprentice ()
  ((confirm-reload :initarg :confirm-reload
                   :accessor confirm-reload
                   :initform t)))

(defmethod System-apprentice-load ((ap system-apprentice) file)
  (alexandria:when-let* ((file file)
                         (system (source-file-system-name file)))
    (emacs-message (format nil "Loading ~A." system))
    (asdf:load-system system)))

;;; Wow, many hoops, such jumping.
(defmethod system-apprentice-reload ((ap system-apprentice) file)
  (alexandria:when-let* ((file file)
                         (system (source-file-system-name file))
                         (confirm (if (confirm-reload ap)
                                      (eval-in-emacs
                                       `(member
                                         (read-key "Restart lisp? (Y/n)")
                                         '(13 121))) ; RET or y
                                      t))
                         (load-string
                          (let ((*package* (find-package :keyword)))
                            (prin1-to-string
                             `(asdf:load-system ',system)))))
    ;; Lexical binding not available.
    (eval-in-emacs
     `(progn
        ;; One-time hook
        (setf (symbol-function 'apprentice-example-system-reload-hook)
              (lambda ()
                (remove-hook 'slime-connected-hook
                             'apprentice-example-system-reload-hook)
                (slime-eval-async
                 (car (read-from-string ,load-string))
                 (lambda (result)
                   (with-current-buffer (get-file-buffer ,file)
                     (slime-sync-package-and-default-directory))))))
        (run-with-timer
         0.1 nil
         (lambda ()
           (add-hook 'slime-connected-hook
                     'apprentice-example-system-reload-hook
                     99)
           (slime-restart-inferior-lisp)))))
    t))

(defmethod system-apprentice-load-current ((ap system-apprentice))
  (system-apprentice-load
   ap (buffer-context-property :filename)))

(defmethod system-apprentice-reload-current ((ap system-apprentice))
  (system-apprentice-reload
   ap (buffer-context-property :filename)))

(defmethod describe-with-apprentice ((ap system-apprentice)
                                     (object looking-at-character)
                                     stream)
  (let ((*standard-output* stream))
    (alexandria:when-let ((file (buffer-context-property :filename)))
      (princ "System: ")
      (put-lisp-button-here ap
                            "[LOAD]"
                            #'system-apprentice-load-current)
      (princ " ")
      (put-lisp-button-here ap
                            "[RELOAD]"
                            #'system-apprentice-reload-current)
      t)))

;;; Finding system files.

(defvar *System-classes-using-subsystems*
  (list "package-inferred-system"
        (lambda (class)
          (alexandria:starts-with-subseq
           "extensible-inferred-system:"
           class)))
  "List of strings or predicates that tell us which ASDF system
classes support inferred subsystems.")

(defun system-class-uses-subsystems-p (class)
  (some (lambda (pred)
          (typecase pred
            (string (equal class pred))
            (t (funcall pred class))))
        *system-classes-using-subsystems*))

(defvar *probe-system-file-max-levels* 3)

(defun probe-system-file (source-file
                          &optional
                          (tries-left *probe-system-file-max-levels*)
                          (dir (pathname-directory source-file)))
  "Probes for an asd file in the same directory as SOURCE-FILE, and then
its parent directories (for a maximum of TRIES-LEFT levels). Signals
an error if more than one asd file is found in a directory.

So for a file /aa/bb/cc/dd.lisp, these directories will be searched,
in order (when *probe-system-file-max-levels* = 3):

/aa/bb/cc/*.asd
/aa/bb/*.asd
/aa/*.asd"
  (assert (<= 0 tries-left))            ; Not negative
  (assert (eq :absolute (first dir)))
  (when (< 0 tries-left)
    (let ((asds (directory (make-pathname :type "asd"
                                          :name :wild
                                          :directory dir))))
      (cond ((< 1 (length asds))
             (error "More than one asd file found."))
            ((= 1 (length asds))
             (first asds))
            ((= 1 (length dir))         ; (:absolute) -> done
             nil)
            (t (probe-system-file
                source-file
                (1- tries-left)
                (butlast dir)))))))

;;; Read without interning.

(defmethod eclector.reader:evaluate-expression
    ((client (eql 'read-no-intern)) expression)
  expression)

(defmethod eclector.reader:interpret-symbol
    ((client (eql 'read-no-intern))
     input-stream
     package-indicator
     symbol-name
     internp)
  (let ((pkg (case package-indicator
               (:current *package*)
               (:keyword (find-package :keyword))
               ((nil) ;; Uninterned symbol
                (return-from eclector.reader:interpret-symbol
                  (make-symbol symbol-name)))
               (t package-indicator))))
    (multiple-value-bind (sym status)
        (find-symbol symbol-name pkg)
      (if status
          sym
          (make-symbol symbol-name)))))

(defun probe-system-class-name (asd-file &optional downcasep)
  "Returns the :CLASS keyword argument of the first system defined in
ASD-FILE."
  (flet ((downcase-maybe (x)
           (if (and downcasep
                    (not (null x))
                    (or (stringp x)
                        (symbolp x)))
               (string-downcase x)
               x)))
    (alexandria:with-input-from-file (s asd-file)
      (let* ((eclector.base:*client* 'read-no-intern)
             (*package* (find-package :asdf-user))
             (defsystem (find-symbol "DEFSYSTEM" "ASDF")))
        (loop :for form = (eclector.reader:read s nil s)
              :until (eq form s)
              :when (and (consp form)
                         (eq defsystem (car form)))
              :do (destructuring-bind (defsystem system-name
                                        &key class &allow-other-keys)
                      form
                    (declare (ignore defsystem system-name))
                    (return-from probe-system-class-name
                      (downcase-maybe class))))))))

;;; Will be called by determine-system-name, if necessary.
(defun determine-subsystem-name (source-file asd-file)
  "Computes the name of an ASDF subsystem that corresponds to
SOURCE-FILE in the context of ASD-FILE. Example:

(determine-subsystem-name \"/myapp/util/macros.lisp\"
                          \"/myapp/myapp.asd\")
=>  \"myapp/util/macros\""
  (let ((namestring (enough-namestring
                     source-file
                     (directory-namestring asd-file))))
    (concatenate 'string
                 (pathname-name asd-file)
                 "/"
                 (directory-namestring namestring)
                 (pathname-name namestring))))

(defun determine-system-name (source-file asd-file)
  (declare (ignore source-file))
  (pathname-name asd-file))

(defun source-file-system-name (source-file)
  "Tries to find the name of an ASDF system that, when loaded, will
result in SOURCE-FILE being loaded."
  (alexandria:when-let* ((asd (probe-system-file source-file)))
    (let ((system-class (probe-system-class-name asd t)))
      (if (system-class-uses-subsystems-p system-class)
          (determine-subsystem-name source-file asd)
          (determine-system-name source-file asd)))))
