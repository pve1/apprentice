;;;; Requires
;;;;   apprentice
;;;;   cl-interpol
;;;;   apply-argv
;;;;   "buttons"
;;;;   "emacs"

(cl-interpol:enable-interpol-syntax)

(in-package :apprentice) cx

;;; Making executable things

;;; TODO:
;;; - More implementations
;;; - Name switches
;;; - Output location (CWD, ~/bin)
;;; - Support creating interactive cores
;;; - Standalone fasl loader, no ASDF
;;; - Also support executables with "subcommands", like git status|pull|...

(defclass system-exec-apprentice ()
  ((modes :initarg :modes
          :accessor modes
          :initform '(script
                      shell-script
                      core))
   (current-mode :initarg :current-mode
                 :accessor current-mode
                 :initform 'script)
   (implementations :initarg :implementations
                    :accessor implementations
                    :initform '(system-sbcl))
   (current-implementation :initarg :current-implementation
                           :accessor current-implementation
                           :initform 'system-sbcl))
  (:documentation ""))

(defclass system-executable ()
  ((output :initarg :output
           :accessor output
           :initform nil)
   (filename :initarg :filename
             :accessor filename
             :initform nil))
  (:documentation ""))

(defclass system-core (system-executable)
  ((build-command :initarg :build-command
                  :accessor build-command
                  :initform nil))
  (:documentation ""))

(defclass system-script (system-executable)
  ()
  (:documentation ""))

(defclass system-shell-script (system-executable)
  ()
  (:documentation ""))

(defclass lisp-implementation ()
  ((binary-path :initarg :binary-path
                :accessor binary-path
                :initform nil)))

(defclass system-sbcl (lisp-implementation)
  ((save-core-arguments :initarg :save-core-arguments
                        :accessor save-core-arguments
                        :initform '(:compression t
                                    :executable t)))
  (:default-initargs :binary-path "/usr/bin/sbcl"))

(defmethod describe-with-apprentice ((ap system-exec-apprentice)
                                     (object symbol)
                                     stream)
  nil)

(defmethod describe-with-apprentice ((ap system-exec-apprentice)
                                     (object symbol)
                                     stream)
  (alexandria:when-let* ((file (buffer-context-property :filename))
                         (system (source-file-system-name file)))
    (let ((*standard-output* stream))
      (princ "Modes: ")
      (dolist (mode (modes ap))
        (put-lisp-button-here
         ap (format nil "[~A] " mode)
         (lambda (appr)
           (set-temporary-apprentice appr)
           (setf (current-mode appr) mode)
           (emacs-message mode))
         :redisplay t))
      (terpri)
      (finish-output)
      (let ((exec (make-system-executable
                   ap
                   (make-instance (current-implementation ap))
                   (current-mode ap)
                   :entry-point object
                   :required-system system)))
        (print (show-system-executable exec))
        (fresh-line)
        (terpri)
        (put-lisp-button-here
         ap "[MAKE-IT-SO]"
         (lambda (x)
           (make-it-so exec)))
        t))))

(defgeneric show-system-executable (exec)
  (:method (exec) exec)
  (:documentation ""))

(defgeneric make-system-executable (apprentice
                                    implementation
                                    mode
                                    &key entry-point
                                         required-system)
  (:documentation ""))

(defun ensure-suffix-sys (string suffix)
  (if (alexandria:ends-with-subseq suffix string)
      string
      (concatenate 'string string suffix)))

(defmethod make-system-executable ((ap system-exec-apprentice)
                                   (implementation system-sbcl)
                                   (mode (eql 'core))
                                   &key entry-point
                                        required-system)
  (let* ((entry-point-name (string-downcase entry-point))
         (output (ensure-suffix-sys entry-point-name ".core"))
         (save-form `(sb-ext:save-lisp-and-die
                      ,output
                      :toplevel
                      (lambda ()
                        (apply #',entry-point
                               (apply-argv:parse-argv*
                                (apply-argv:get-argv))))
                      :executable t
                      :compression t))
         (command (list (binary-path implementation)
                        "--noinform"
                        "--non-interactive"
                        "--eval" "(require '#:asdf)"
                        "--eval" "(asdf:load-system '#:apply-argv)"
                        "--eval" #?{(asdf:load-system '#:$(required-system))}
                        "--eval" (with-standard-io-syntax
                                   (prin1-to-string save-form)))))
    (make-instance 'system-core
      :output output
      :build-command command)))

(defmethod show-system-executable ((exec system-core))
  (build-command exec))

(defmethod make-it-so ((exec system-core))
  (emacs-message "Please wait.")
  (uiop:run-program (build-command exec) :output *debug-io*)
  (emacs-message "Done."))

(defmethod make-system-executable ((ap system-exec-apprentice)
                                   (implementation system-sbcl)
                                   (mode (eql 'script))
                                   &key required-system
                                        entry-point)
  (let* ((entry-point-string
           (string-downcase
            (let ((*package* (find-package :keyword)))
              (prin1-to-string entry-point))))
         (entry-point-name
           (string-downcase (symbol-name entry-point)))
         (filename (ensure-suffix-sys
                    entry-point-name ".lisp")))
    (make-instance 'system-script
      :output (format-suggestion
               #?{
               ;; ${filename}

               (sb-ext:disable-debugger)
               (require '#:asdf)
               (asdf:load-system '#:apply-argv)
               (asdf:load-system '#:$(required-system))
               (apply #'${entry-point-string}
                      (apply-argv:parse-argv*
                       (apply-argv:get-argv)))
               (sb-ext:exit)
               })
      :filename filename)))

(defmethod show-system-executable ((exec system-script))
  (output exec))

(defmethod make-it-so ((exec system-script))
  (alexandria:with-output-to-file (f (filename exec))
    (princ (output exec) f))
  (emacs-message "Done."))

(defmethod make-system-executable ((ap system-exec-apprentice)
                                   (implementation system-sbcl)
                                   (mode (eql 'shell-script))
                                   &key required-system
                                        entry-point)
  (let* ((entry-point-string
           (string-downcase
            (let ((*package* (find-package :keyword)))
              (prin1-to-string entry-point))))
         (entry-point-name
           (string-downcase (symbol-name entry-point)))
         (filename (ensure-suffix-sys entry-point-name
                                      ".sh")))
    (make-instance 'system-shell-script
      :output (format-suggestion
               #?{
               #!/bin/sh

               # ${filename}

               ${(binary-path implementation)} \\
               --noinform \\
               --non-interactive \\
               --eval "(require '#:asdf)" \\
               --eval "(asdf:load-system '#:apply-argv)" \\
               --eval "(asdf:load-system '#:$(required-system))" \\
               --eval "(apply #'${entry-point-string}
                        (apply-argv:parse-argv*
                         (apply-argv:get-argv)))" \\
               --eval "(sb-ext:exit)" \\
               "$@"
               })
      :filename filename)))

(defmethod show-system-executable ((exec system-shell-script))
  (output exec))

(defmethod make-it-so ((exec system-shell-script))
  (alexandria:with-output-to-file (f (filename exec))
    (princ (output exec) f))
  (uiop:run-program (list "chmod" "u+x" (filename exec)))
  (emacs-message "Done."))
