;;;; Requires
;;;;   apprentice
;;;;   cl-interpol
;;;;   cl-ppcre
;;;;   apply-argv
;;;;   "buttons"
;;;;   "emacs"

(cl-interpol:enable-interpol-syntax)

(in-package :apprentice) cx

;;; Making executable things

;;; Objective: Make any lisp function available as a lisp script or a
;;; shell command either through a shell script or by saving a core.

;;; TODO:
;;; - More implementations
;;;   - [X] SBCL
;;;   - [/] ECL
;;; - Name switches
;;; - Support creating interactive cores
;;; - Standalone fasl loader, no ASDF
;;; - [X] Output location (CWD, ~/bin)
;;; - [X] Also support executables with "subcommands", like git status|pull|...

;;; Apprentice class

(defclass exec-apprentice ()
  ((modes :initarg :modes
          :accessor modes)
   (current-mode :initarg :current-mode
                 :accessor current-mode)
   (implementations :initarg :implementations
                    :accessor implementations)
   (current-implementation :initarg :current-implementation
                           :accessor current-implementation)
   (output-directory-alternatives :initarg :output-directory-alternatives
                                  :accessor output-directory-alternatives)
   (current-output-directory :initarg :current-output-directory
                             :accessor current-output-directory)
   (argv-parsing-methods :initarg :argv-parsing-methods
                         :accessor argv-parsing-methods)
   (current-argv-parsing-method :initarg :current-argv-parsing-method
                                :accessor current-argv-parsing-method)
   (output-extension-alternatives :initarg :output-extension-alternatives
                                  :accessor output-extension-alternatives)
   (current-output-extension-alternative
    :initarg :current-output-extension-alternative
    :accessor current-output-extension-alternative))
  (:default-initargs
   :modes (list (option-exe 'shell-script 'exec-shell-script)
                (option-exe 'lisp-script 'exec-lisp-script)
                (option-exe 'core 'exec-core))
   :implementations (list (option-exe 'sbcl 'exec-sbcl)
                          (option-exe 'ecl 'exec-ecl))
   :argv-parsing-methods (list (option-exe 'single 'single)
                               (option-exe 'subcommands 'subcommands))
   :output-directory-alternatives
   (list (option-exe "./" 'cwd-exe)
         (option-exe "~/bin/" (in-homedir-exe "bin/"))
         (option-exe "~/.local/bin/" (in-homedir-exe ".local/bin/")))
   :output-extension-alternatives
   (list (option-exe 'yes t)
         (option-exe 'no nil)))
  (:documentation ""))

(defstruct (option-exe (:type list)
                       (:constructor option-exe
                           (label value)))
  label value)

(defun in-homedir-exe (pathname)
  (merge-pathnames pathname (user-homedir-pathname)))

(defun cwd-exe ()
  (directory-namestring
   (buffer-context-property :filename)))

(defmethod current-output-directory ((ap exec-apprentice))
  (let ((val (slot-value ap 'current-output-directory)))
    (typecase val
      ((or function symbol) (funcall val))
      (t val))))

(defmethod initialize-instance :after ((s exec-apprentice) &key)
  ;; Initialize current options to the first element of each list.
  (loop :for (all current)
        :in '((modes current-mode)
              (implementations current-implementation)
              (argv-parsing-methods current-argv-parsing-method)
              (output-directory-alternatives
               current-output-directory)
              (output-extension-alternatives
               current-output-extension-alternative))
        :do (unless (slot-boundp s current)
              (setf (slot-value s current)
                    (option-exe-value (first (funcall all s)))))))

(defmethod exec-present-alternatives ((ap exec-apprentice)
                                      label
                                      option-reader
                                      current-writer)
  (princ label)
  (dolist (option (funcall option-reader ap))
    (let ((olabel (option-exe-label option))
          (ovalue (option-exe-value option)))
      (put-lisp-button-here
       ap (format nil "[~A] " olabel)
       (lambda (appr)
         (set-temporary-apprentice appr)
         (funcall current-writer ovalue ap)
         (emacs-message olabel))
       :redisplay t)))
  (terpri))

(defmethod describe-with-apprentice ((ap exec-apprentice)
                                     (object symbol)
                                     stream)
  (alexandria:when-let* ((file (buffer-context-property :filename))
                         (system (source-file-system-name file)))
    (let ((*standard-output* stream))
      (terpri)
      ;; Display options
      (exec-present-alternatives
       ap "Implementation: "
       #'implementations
       #'(setf current-implementation))
      (exec-present-alternatives
       ap "Mode: "
       #'modes
       #'(setf current-mode))
      (exec-present-alternatives
       ap "Argv parsing: "
       #'argv-parsing-methods
       #'(setf current-argv-parsing-method))
      (exec-present-alternatives
       ap "Output directory: "
       #'output-directory-alternatives
       #'(setf current-output-directory))
      (exec-present-alternatives
       ap "Output extension: "
       #'output-extension-alternatives
       #'(setf current-output-extension-alternative))
      ;; Check that output directory exists.
      (unless (probe-file (current-output-directory ap))
        (format t "~&~%Directory ~S doesn't exist!"
                (current-output-directory ap))
        (return-from describe-with-apprentice nil))
      (finish-output)
      (let* ((exec
               (make-executable
                ap
                :mode (make-instance (current-mode ap))
                :argv-parser (current-argv-parsing-method ap)
                :required-system system
                :implementation (make-instance (current-implementation ap))
                :entry-point object
                :output-directory (current-output-directory ap)
                :extensionp (current-output-extension-alternative ap))))
        (terpri)
        ;; Display executable.
        (princ (show-executable exec))
        (fresh-line)
        (terpri)
        (put-lisp-button-here
         ap "[MAKE-IT-SO]"
         (lambda (x)
           (make-it-so exec)))
        t))))

;;; Lisp implementations

(defgeneric binary-path (implementation)
  (:documentation ""))

(defgeneric static-arguments (implementation)
  (:method (implementation) nil)
  (:documentation ""))

(defgeneric name (implementation)
  (:documentation ""))

(defclass exec-lisp-implementation ()
  ((binary-path :initarg :binary-path
                :accessor binary-path
                :initform nil)
   (static-arguments :initarg :static-arguments
                     :accessor static-arguments
                     :initform nil)
   (name :initarg :name
         :accessor name
         :initform nil)))

(defmethod initialize-instance :after ((e exec-lisp-implementation)
                                       &key name)
  (unless name
    (setf (name e) (pathname-name (binary-path e)))))

(defclass exec-sbcl (exec-lisp-implementation)
  ()
  (:default-initargs
   :binary-path "/usr/bin/sbcl"
   :static-arguments (list "--noinform"
                           "--non-interactive")))

(defclass exec-ecl (exec-lisp-implementation)
  ()
  (:default-initargs
   :binary-path "/usr/local/bin/ecl"
   :static-arguments nil))

;;; Build output

(defclass executable ()
  ((output :initarg :output
           :accessor executable-output
           :initform nil)
   (output-path :initarg :output-path
                :accessor executable-output-path
                :initform nil)))

(defclass exec-core (executable) ())
(defclass exec-lisp-script (executable) ())
(defclass exec-shell-script (executable) ())

(defmethod print-object ((s executable) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~S" (executable-output s)))
  s)

(defgeneric show-executable (executable)
  (:method (executable)
    (executable-output executable))
  (:method ((executable exec-core))
    (let ((*print-right-margin* 1))
      (prin1-to-string (executable-output executable)))))

;;; Writing the output to disk.

(defmethod make-it-so ((exec exec-core))
  (emacs-message "Please wait.")
  (uiop:run-program (executable-output exec)
                    :output *debug-io*
                    :error-output *debug-io*)
  (emacs-message "Done."))

(defmethod make-it-so ((exec exec-shell-script))
  (let ((file (executable-output-path exec)))
    (alexandria:with-output-to-file (f file)
      (princ (executable-output exec) f))
    (uiop:run-program (list "chmod" "u+x" file))
    (emacs-message "Done.")))

(defmethod make-it-so ((exec exec-lisp-script))
  (alexandria:with-output-to-file
      (f (executable-output-path exec))
    (princ (executable-output exec) f))
  (emacs-message "Done."))

;;; Building the output (scripts or core).

(defvar *exec-build-context*)

(defmethod set-exe (symbol value)
  (setf (gethash symbol *exec-build-context*) value))

(defmethod get-exe (symbol)
  (gethash symbol *exec-build-context*))

;;; Mode is an empty "prototype" instance of what is being built.

;;; Make-executable is stand-alone, in the sense that it doesn't know
;;; anything about apprentices. It simply returns the build ouput
;;; based on the keyword parameters below.

(defmethod make-executable (apprentice ; Can be anything
                            &key mode
                                 output-directory
                                 entry-point
                                 implementation
                                 required-system
                                 argv-parser
                                 (extensionp t))
  (let ((*exec-build-context* (make-hash-table :test 'equal)))
    (when (pathnamep output-directory)
      (setf output-directory (namestring output-directory)))
    (assert (alexandria:ends-with-subseq "/" output-directory))
    ;; Setup initial context.
    (set-exe 'entry-point entry-point)
    (set-exe 'entry-point-string (string-downcase entry-point))
    (set-exe 'qualified-entry-point-string
             (let ((*package* (find-package :keyword)))
               (string-downcase
                (prin1-to-string entry-point))))
    (set-exe 'implementation implementation)
    (set-exe 'required-system required-system)
    (set-exe 'argv-parser argv-parser)
    (set-exe 'extensionp extensionp)
    (set-exe 'output-directory output-directory)
    ;; Go!
    (build-output-exe implementation mode)))

;;; Utilities needed by the build-* functions.

(defun trim-lines-left-exe (string)
  (cl-ppcre:regex-replace-all
   (cl-ppcre:create-scanner "^ +" :multi-line-mode t)
   string
   ""))

(defun escape-single-quote-sh-exe (string)
  (cl-ppcre:regex-replace-all "'" string "'\\\\''"))

(defun make-shell-argument-sh-exe (flag string)
  (with-output-to-string (*standard-output*)
    (princ flag)
    (princ " '")
    (princ (escape-single-quote-sh-exe string))
    (princ "'")))

;;; General methods

(defmethod build-output-path-exe (impl mode &key extension)
  (namestring
   (merge-pathnames
    (concatenate 'string
                 (get-exe 'entry-point-string)
                 (if (and (get-exe 'extensionp)
                          extension)
                     extension
                     ""))
    (truename (get-exe 'output-directory)))))

(defmethod build-toplevel-form-string-exe (impl mode)
  (let ((argv-parser-form
          (case (get-exe 'argv-parser)
            (single "(apply-argv:parse-argv* (apply-argv:get-argv))")
            (subcommands "(apply-argv:parse-argv (apply-argv:get-argv))")
            (t (get-exe 'argv-parser)))))
    #?"(apply #'${(get-exe 'qualified-entry-point-string)} ${argv-parser-form})"))

(defmethod build-output-exe (impl (mode exec-core))
  (make-instance 'exec-core
    :output (append (list (binary-path impl))
                    (static-arguments impl)
                    (build-eval-arguments-exe impl mode))))

;;; Lisp core sbcl

(defmethod build-eval-arguments-exe ((impl exec-sbcl) (mode exec-core))
  (list "--eval" "(require '#:asdf)"
        "--eval" "(asdf:load-system '#:apply-argv)"
        "--eval" #?{(asdf:load-system '#:${(get-exe 'required-system)})}
        "--eval" (build-save-core-form-string-exe impl mode)))

(defmethod build-save-core-form-string-exe ((impl exec-sbcl) (mode exec-core))
  (let ((output-path (build-output-path-exe impl mode :extension ".core"))
        (toplevel-form-string
          (build-toplevel-form-string-exe impl mode)))
    #?{\
    (sb-ext:save-lisp-and-die \
     "${output-path}" \
     :toplevel (lambda () \
                 ${toplevel-form-string}) \
     :executable t \
     :compression t)}))

;;; Lisp core ecl

(defmethod build-save-core-form-string-exe ((impl exec-ecl) (mode exec-core))
  (let ((output-path (build-output-path-exe impl mode :extension ".core"))
        (toplevel-form-string
          (build-toplevel-form-string-exe impl mode)))
    #?{\
    (let ((object-files
            (remove-if-not (lambda (x)
                             (equal "o" (pathname-type x)))
                           (remove-duplicates
                            (append
                             (apprentice::exec-fasls-to-load-system
                              "${(get-exe 'required-system)}")
                             (apprentice::exec-fasls-to-load-system
                              "apply-argv"))
                            :test #'equal
                            :from-end t))))
      (c:build-program
       "${output-path}"
       :lisp-files object-files
       :epilogue-code '(progn
                        ${toplevel-form-string}
                        (si:exit))))}))

(defmethod build-eval-arguments-exe ((impl exec-ecl) (mode exec-core))
  (list "--eval" "(asdf:load-system '#:apply-argv)"
        "--eval" "(asdf:load-system '#:apprentice/exec-apprentice)"
        "--eval" #?{(asdf:load-system '#:${(get-exe 'required-system)})}
        "--eval" (build-save-core-form-string-exe impl mode)
        "--eval" "(si:quit)"))

;;; Shell script general

(defmethod build-output-exe (impl (mode exec-shell-script))
  (let* ((output-path (build-output-path-exe impl mode :extension ".sh"))
         (eval-arguments (build-eval-arguments-exe impl mode))
         (static-arguments (static-arguments impl))
         (cl-interpol:*list-delimiter* #?{ \\\n})
         ;; Slime gets confused by "# ", inside the interpol string,
         ;; but escaping it works.
         (script #?{\
           #!/bin/sh
           \# ${output-path}
           ${(binary-path impl)} \\
           @{ static-arguments } \\
           @{ eval-arguments } \\
           "$@"
           }))
    (make-instance 'exec-shell-script
      :output-path output-path
      :output (trim-lines-left-exe script))))

;;; Shell script sbcl

(defmethod build-eval-arguments-exe ((impl exec-sbcl) (mode exec-shell-script))
  (mapcar (lambda (x)
            (make-shell-argument-sh-exe "--eval" x))
          (list #?"(require '#:asdf)"
                #?"(asdf:load-system '#:apply-argv)"
                #?"(asdf:load-system '#:${(get-exe 'required-system)})"
                #?"${(build-toplevel-form-string-exe impl mode)}"
                #?"(sb-ext:exit)")))

;;; Shell script ecl

(defmethod build-toplevel-form-string-exe ((impl exec-ecl) (mode exec-shell-script))
  (let ((argv-parser-form
          (case (get-exe 'argv-parser)
            (single #?{(let ((arguments (nthcdr (position "--" (ext:command-args) :test #'equal) (ext:command-args)))) (apply-argv:parse-argv* arguments))})
            (subcommands #?{(let ((arguments (nthcdr (position "--" (ext:command-args) :test #'equal) (ext:command-args)))) (apply-argv:parse-argv arguments))})
            (t (get-exe 'argv-parser)))))
    #?"(apply #'${(get-exe 'qualified-entry-point-string)} ${argv-parser-form})"))

(defmethod build-eval-arguments-exe ((impl exec-ecl) (mode exec-shell-script))
  (append (mapcar (lambda (x)
                    (make-shell-argument-sh-exe "--eval" x))
                  (list #?"(asdf:load-system '#:apply-argv)"
                        #?"(asdf:load-system '#:${(get-exe 'required-system)})"
                        #?"${(build-toplevel-form-string-exe impl mode)}"
                        #?"(si:quit)"))
          (list "--")))

;;; Lisp script

(defmethod build-output-exe ((impl exec-sbcl) (mode exec-lisp-script))
  (let* ((output-path
           (build-output-path-exe
            impl mode
            :extension ".script.lisp"))
         (toplevel-form-string
           (build-toplevel-form-string-exe impl mode))
         (cl-interpol:*list-delimiter* #?{ \\\n})
         (script #?{\
           ;; ${output-path}
          (sb-ext:disable-debugger)
          (require '#:asdf)
          (asdf:load-system '#:apply-argv)
          (asdf:load-system '#:${(get-exe 'required-system)})
          ${toplevel-form-string}
          (sb-ext:exit)
          }))
    (make-instance 'exec-lisp-script
      :output-path output-path
      :output (trim-lines-left-exe script))))

;;; System dependencies

(defun exec-system-fasl-files (system-name)
  (loop :for c :in (asdf:required-components
                    (asdf:find-system system-name))
        :when (typep c 'asdf:cl-source-file)
        :append (asdf:output-files 'asdf:compile-op c)))

(defun exec-system-dependencies (system-name)
  (labels ((dependencies (system-name)
             (let* ((system (asdf:find-system system-name))
                    (deps (asdf:system-depends-on system))
                    ;; Handles (:feature ...)
                    (resolved-deps
                      (alexandria:mappend
                       (lambda (s)
                         (let ((dep (asdf/find-component:resolve-dependency-spec
                                     system s)))
                           (when dep
                             (list (asdf:component-name dep)))))
                       deps)))
               (append resolved-deps
                       (alexandria:mappend #'dependencies resolved-deps)))))
    (reverse (remove-duplicates (dependencies system-name) :test #'string=))))

(defun exec-fasls-to-load-system (system)
  (let ((deps (exec-system-dependencies system)))
    ;; Check for 'require' systems.
    (dolist (sysname deps)
      (let ((sys (asdf:find-system sysname)))
        (when (typep sys 'asdf:require-system)
          (warn "System ~S must be 'required', ignoring."
                sysname))))
    (loop :for sys :in (append deps (list system))
          :append (exec-system-fasl-files sys))))

