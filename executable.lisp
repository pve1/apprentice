;;;; Requires
;;;;   apprentice
;;;;   cl-interpol
;;;;   cl-ppcre
;;;;   "emacs"
;;;;   "suggest-apprentice"

;; Note: suggest-apprentice only for normalize-indentation

(cl-interpol:enable-interpol-syntax)

(in-package :apprentice) cx

;;; Lisp implementations

(defgeneric binary-path (implementation)
  (:documentation ""))

(defgeneric static-arguments (implementation)
  (:method (implementation) nil)
  (:documentation ""))

(defgeneric name (implementation)
  (:documentation ""))

(defun implementation-from-pathname-exe (pathname-designator)
  (let ((namestring (namestring pathname-designator)))
    (alexandria:switch ((pathname-name pathname-designator)
                        :test #'equal)
      ("sbcl" (make-instance 'exec-sbcl :binary-path namestring))
      ("ecl" (make-instance 'exec-ecl :binary-path namestring)))))

(defun coerce-to-implementation-exe (thing)
  (typecase thing
    ((or string pathname)
     (implementation-from-pathname-exe thing))
    (exec-lisp-implementation
     thing)))

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
    (setf (name e) (string-upcase
                    (pathname-name (binary-path e))))))

(defclass exec-sbcl (exec-lisp-implementation) ()
  (:default-initargs
   :binary-path "/usr/bin/sbcl"
   :static-arguments (list "--noinform"
                           "--non-interactive")))

(defclass exec-ecl (exec-lisp-implementation) ()
  (:default-initargs
   :binary-path "/usr/bin/ecl"
   :static-arguments (list "-q")))

;;; Build output (Modes)

(defclass build ()
  ((output :initarg :output
           :accessor build-output
           :initform nil)
   (output-path :initarg :output-path
                :accessor build-output-path
                :initform nil))
  (:documentation
   "Something that can be turned into a shell command using the function
 MAKE-IT-SO."))

(defclass exec-lisp-script (build) ())
(defclass exec-shell-script (build) ())
(defclass exec-core (build) ())
(defclass exec-basic-core (exec-core) ()) ; default toplevel
(defclass exec-program-core (exec-core) ()) ; user-specified toplevel
(defclass exec-slim-program-core (exec-program-core) ())

(defmethod print-object ((s build) stream)
  (print-unreadable-object (s stream :type t)
    (let ((*print-right-margin* nil))
      (format stream "~S" (build-output s))))
  s)

(defgeneric show-build (build)
  (:method ((build null))
    "")
  (:method (build)
    (build-output build))
  (:method ((build exec-core))
    (let ((*print-right-margin* 1))
      (prin1-to-string (build-output build)))))

;;; Writing the output to disk.

(defmethod make-it-so ((exec exec-core))
  (emacs-message "Please wait.")
  (uiop:run-program (build-output exec)
                    :output *debug-io*
                    :error-output *debug-io*)
  (emacs-message "Done."))

(defmethod make-it-so ((exec exec-slim-program-core))
  (emacs-message "Please wait.")
  (uiop:run-program (build-output exec)
                    :output *debug-io*
                    :error-output *debug-io*)
  (emacs-message "Done."))

(defmethod make-it-so ((exec exec-shell-script))
  (let ((file (build-output-path exec)))
    (alexandria:with-output-to-file (f file)
      (princ (build-output exec) f))
    (uiop:run-program (list "chmod" "u+x" file))
    (emacs-message "Done.")))

(defmethod make-it-so ((exec exec-lisp-script))
  (alexandria:with-output-to-file
      (f (build-output-path exec))
    (princ (build-output exec) f))
  (emacs-message "Done."))

;;; Building the output (scripts or core).

(defvar *exec-build-context*)

(defmethod set-exe (symbol value)
  (setf (gethash symbol *exec-build-context*) value))

(defmethod get-exe (symbol)
  (gethash symbol *exec-build-context*))

;;; Mode is an empty "prototype" instance of what is being built.

;;; Make-build is stand-alone, in the sense that it doesn't know
;;; anything about apprentices. It simply returns the build ouput
;;; based on the keyword parameters below.

(defmethod make-build (apprentice       ; Can be anything
                       &key mode
                            output-directory
                            entry-point
                            implementation
                            required-system
                            additional-systems
                            eval-args
                            arg-parser
                            package
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
    (set-exe 'additional-systems (alexandria:ensure-list
                                  additional-systems))
    (set-exe 'arg-parser arg-parser)
    (set-exe 'package package)
    (set-exe 'extensionp extensionp)
    (set-exe 'output-directory output-directory)
    (set-exe 'eval-args eval-args)
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

(defun stitch-exe (element list)
  (loop :for i :in list
        :collect element
        :collect i))

;; Turns a string into a shell string, escaping single quotes.
;; So: "abc'123" -> 'abc'\''123'
(defun make-shell-argument-sh-exe (flag string)
  (with-output-to-string (*standard-output*)
    (princ flag)
    (princ " '")
    (princ (escape-single-quote-sh-exe string))
    (princ "'")))

;; Creates the output pathname.
(defun build-output-path-helper-exe (name
                                     output-dir
                                     extensionp
                                     extension)
  (namestring
   (merge-pathnames
    (concatenate 'string name (if (and extensionp
                                       extension)
                                  extension
                                  ""))
    (truename output-dir))))

(defun load-required-system-exe ()
  #?{(asdf:load-system "${(get-exe 'required-system)}")})

(defun load-additional-systems-exe ()
  (mapcar (lambda (x)
            #?{(asdf:load-system "${(string-downcase x)}")})
          (get-exe 'additional-systems)))

;; Creates the toplevel string.
(defun toplevel-helper-exe (impl mode &key arguments)
  (unless arguments
    (setf arguments (exec-command-line-args-string-for-mode
                     impl mode)))
  (let* ((arg-parser (get-exe 'arg-parser))
         (arg-parser-form
           (typecase arg-parser
             (null "()")
             (string arg-parser)
             (cons
              (case (first arg-parser)
                (call-with-args
                 #?{(${(second arg-parser)} ${arguments})})
                (t "()")))
             (symbol
              (case arg-parser
                (argv (exec-get-argv-string impl))
                (unparsed arguments)
                (none #?{nil}))))))
    #?"(apply #'${(get-exe 'qualified-entry-point-string)} ${arg-parser-form})"))

;;; General

(defmethod build-output-exe (impl mode)
  nil)

(defmethod build-output-path-exe (impl mode &key extension)
  (build-output-path-helper-exe (get-exe 'entry-point-string)
                                (get-exe 'output-directory)
                                (get-exe 'extensionp)
                                extension))

(defmethod build-toplevel-form-string-exe (impl mode)
  (toplevel-helper-exe impl mode))

;;; Shell script general

(defmethod build-output-exe (impl (mode exec-shell-script))
  (let* ((output-path (build-output-path-exe impl mode :extension ".sh"))
         (binary-path (binary-path impl))
         (eval-arguments (build-eval-arguments-exe impl mode))
         (static-arguments (static-arguments impl))
         (cl-interpol:*list-delimiter* #?{ \\\n})
         ;; Slime gets confused by "# ", inside the interpol string,
         ;; but escaping it works.
         (script #?{\
           #!/bin/sh
           \# ${ output-path }
           ${ binary-path } \\
           @{ static-arguments } \\
           @{ eval-arguments } \\
           ${ (end-toplevel-options-exe impl) } \\
           "$@"
           }))
    (make-instance 'exec-shell-script
      :output-path output-path
      :output (trim-lines-left-exe script))))

(defmethod end-toplevel-options-exe (impl)
  "--")

(defmethod end-toplevel-options-exe ((impl exec-sbcl))
  "--end-toplevel-options")

;;; Lisp script general

(defmethod build-output-exe (impl (mode exec-lisp-script))
  (let* ((output-path
             (build-output-path-exe
              impl mode
              :extension "-script.lisp"))
           (toplevel-form-string
             (build-toplevel-form-string-exe impl mode))
           (eval-args (get-exe 'eval-args))
           (cl-interpol:*list-delimiter* #?{ \\\n})
           (script
             (with-output-to-string (*standard-output*)
               (write-line #?{\;\; ${output-path}})
               (write-line (exec-disable-debugger-string impl))
               (write-line (load-required-system-exe))
               (dolist (s (load-additional-systems-exe))
                 (write-line s))
               (dolist (s eval-args)
                 (write-line s))
               (write-line toplevel-form-string)
               (write-line (exec-exit-string impl)))))
      (make-instance 'exec-lisp-script
        :output-path output-path
        :output (trim-lines-left-exe script))))

;;; Core general

;; This is roughly how the build-* functions are used to build a core:

;; 1. build-output-exe (highest level)
;;  2. build-eval-arguments-exe
;;   3. build-save-core-form-string-exe
;;    4. build-toplevel-form-string-exe (lowest level)

(defmethod build-output-exe (impl (mode exec-core))
  (make-instance (class-of mode)
    :output (append (list (binary-path impl))
                    (static-arguments impl)
                    (build-eval-arguments-exe impl mode))))

;;; Shell script SBCL

(defmethod build-eval-arguments-exe ((impl exec-sbcl) (mode exec-shell-script))
  (mapcar (lambda (x)
            (make-shell-argument-sh-exe "--eval" x))
          (alexandria:flatten
           (list #?{(require "ASDF")}
                 (load-required-system-exe)
                 (load-additional-systems-exe)
                 (get-exe 'eval-args)
                 (build-toplevel-form-string-exe impl mode)
                 "(sb-ext:exit)"))))

;;; Basic core SBCL

(defmethod build-eval-arguments-exe ((impl exec-sbcl) (mode exec-basic-core))
  (alexandria:flatten
   (list "--eval" "(setf *load-verbose* t)"
         "--eval" (load-required-system-exe)
         (stitch-exe "--eval" (load-additional-systems-exe))
         (stitch-exe "--eval" (get-exe 'eval-args))
         "--eval" "(setf *load-verbose* nil)"
         "--eval" (build-save-core-form-string-exe impl mode))))

(defmethod build-save-core-form-string-exe ((impl exec-sbcl) (mode exec-basic-core))
  (let ((output-path (build-output-path-exe impl mode :extension ".core")))
    #?{(progn
         (sb-ext:enable-debugger)
         (handler-case (sb-ext:save-lisp-and-die
                        "${output-path}")
           (serious-condition (c)
             (format *error-output* "Condition signalled: ~A" c)
             (sb-ext:exit :code 1))))}))

;;; Program core SBCL

(defmethod build-eval-arguments-exe ((impl exec-sbcl) (mode exec-program-core))
  (alexandria:flatten
   (list "--eval" "(setf *load-verbose* t)"
         "--eval" #?{(require "ASDF")}
         "--eval" (load-required-system-exe)
         (stitch-exe "--eval" (load-additional-systems-exe))
         (stitch-exe "--eval" (get-exe 'eval-args))
         "--eval" "(setf *load-verbose* nil)"
         "--eval" (build-save-core-form-string-exe impl mode))))

(defmethod build-save-core-form-string-exe ((impl exec-sbcl) (mode exec-program-core))
  (let ((output-path (build-output-path-exe impl mode :extension ".core"))
        (toplevel-form-string
          (build-toplevel-form-string-exe impl mode)))
    #?{\
    (sb-ext:save-lisp-and-die \
     "${output-path}" \
     :toplevel (lambda () ${toplevel-form-string}) \
     :save-runtime-options t \
     :executable t \
     :compression t)}))

;;; Slim program core SBCL

;;; This is similar to the ECL program core, i.e. just load the
;;; necessary fasls into a fresh, --no-userinit core. This is done
;;; using two nested calls to run-program.

(defmethod build-eval-arguments-exe ((impl exec-sbcl) (mode exec-slim-program-core))
  (alexandria:flatten
   (list "--eval" #?{(require "ASDF")}
         "--eval" #?{(asdf:load-system "apprentice/system")}
         "--eval" #?{(asdf:load-system "apprentice/executable")}
         ;; Only required-system and additional-systems end up in the
         ;; core.
         "--eval" #?{(asdf:load-system "${(get-exe 'required-system)}")}
         (stitch-exe "--eval" (load-additional-systems-exe))
         (stitch-exe "--eval" (get-exe 'eval-args))
         "--eval" (build-save-core-form-string-exe impl mode))))

(defmethod build-save-core-form-string-exe ((impl exec-sbcl) (mode exec-slim-program-core))
  (let ((output-path (build-output-path-exe impl mode :extension ".core"))
        (toplevel-form-string
          (build-toplevel-form-string-exe impl mode))
        (systems (mapcar 'prin1-to-string
                         (list* (get-exe 'required-system)
                                (get-exe 'additional-systems)))))
    (normalize-indentation
     #?{\
     (multiple-value-bind (all-fasls require-systems)
         (apprentice::fasls-to-load-multiple-systems
          '(@{systems}))
       (let ((code (with-output-to-string (*standard-output*)
                     (print '(in-package "CL-USER"))
                     (print '(setf *load-verbose* t))
                     (dolist (require require-systems)
                       (print `(require ,require)))
                     (dolist (fasl all-fasls)
                       (print `(load ,fasl)))
                     (print '(setf *load-verbose* nil))
                     (print '(sb-ext:save-lisp-and-die
                              "${output-path}"
                              :toplevel (lambda () ${toplevel-form-string})
                              :save-runtime-options t
                              :executable t
                              :compression t)))))
         (apprentice::exec-eval-externally
          (make-instance 'apprentice::exec-sbcl
            :binary-path "${(binary-path impl)}"
            :static-arguments '${(prin1-to-string (static-arguments impl))})
          code
          :no-init t
          :output *debug-io*)))})))

;;; Shell script ECL

(defmethod build-eval-arguments-exe ((impl exec-ecl) (mode exec-shell-script))
  (mapcar (lambda (x)
            (make-shell-argument-sh-exe "--eval" x))
          (alexandria:flatten
           (list (load-required-system-exe)
                 (load-additional-systems-exe)
                 (get-exe 'eval-args)
                 (build-toplevel-form-string-exe impl mode)
                 "(si:exit)"))))

;;; Basic core ECL

;;; Note about ECL: The "cores" (programs) will not work when built
;;; from systems that depend (even implicitly) on libraries for which
;;; we cannot generate a clean list of .o files. Such libraries
;;; include asdf, uiop and swank.

;; A mechanism for manually specifying the necessary .o files is at
;; least needed to make the "cores" work in general. Perhaps as a
;; property of the implementation instance.

(defmethod build-eval-arguments-exe ((impl exec-ecl) (mode exec-basic-core))
  (list "--eval" "(setf *load-verbose* t)"
        "--eval" #?{(asdf:load-system "apprentice/system")}  ; for finding fasls
        "--eval" "(setf *load-verbose* nil)"
        "--eval" (build-save-core-form-string-exe impl mode)
        "--eval" "(si:exit)"))

(defun ecl-save-core-helper-exe (&key systems
                                      output-path
                                      epilogue)
  (assert output-path)
  (assert systems)
  (setf systems (alexandria:ensure-list systems))
  (let* ((epilogue-arg
           (if epilogue
               #?{:epilogue-code ${ epilogue }}
               "")))
    #?{(let* ((systems '${ (prin1-to-string systems) })
              (object-files
                (remove-if-not
                 (lambda (x)
                   (equal "o" (pathname-type x)))
                 (apprentice::fasls-to-load-multiple-systems systems))))
         ;; Ensure compiled.
         (dolist (sys systems)
           (asdf:load-system sys))
         (c:build-program
          "${output-path}"
          :lisp-files object-files
          ${epilogue-arg}))}))

(defmethod build-save-core-form-string-exe ((impl exec-ecl) (mode exec-basic-core))
  (let* ((output-path (build-output-path-exe impl mode :extension ".core"))
         (systems (get-exe 'required-system)))
    (ecl-save-core-helper-exe :systems systems
                              :output-path output-path)))

;;; Program core ECL

(defmethod build-eval-arguments-exe ((impl exec-ecl) (mode exec-program-core))
  (alexandria:flatten
   (list "--eval" "(setf *load-verbose* t)"
         "--eval" #?{(asdf:load-system "apprentice/system")} ; to find fasls
         "--eval" #?{(asdf:load-system "${(get-exe 'required-system)}")}
         (stitch-exe "--eval" (load-additional-systems-exe))
         (stitch-exe "--eval" (get-exe 'eval-args))
         "--eval" "(setf *load-verbose* nil)"
         "--eval" (build-save-core-form-string-exe impl mode)
         "--eval" "(si:exit)")))

(defmethod build-save-core-form-string-exe ((impl exec-ecl) (mode exec-program-core))
  (let ((output-path (build-output-path-exe impl mode :extension ".core"))
        (toplevel-form-string
          (build-toplevel-form-string-exe impl mode))
        (systems (mapcar 'prin1-to-string
                         (list* (get-exe 'required-system)
                                (get-exe 'additional-systems)))))
    #?{\
    (let ((object-files
            (remove-duplicates
             (loop :for system
                   :in '(@{systems})
                   :for fasls =
                      (apprentice::fasls-to-load-system
                       system)
                   :for obj =
                      (remove-if-not
                       (lambda (x)
                         (equal "o" (pathname-type x)))
                       fasls)
                   :append obj)
             :test #'equal
             :from-end t)))
      (c:build-program
       "${output-path}"
       :lisp-files object-files
       :epilogue-code '(progn
                        (handler-case
                            ${toplevel-form-string}
                          (serious-condition (c)
                            (format *error-output* "~&~A~%" c)
                            (si:exit 1)))
                        (si:exit))))}))

;;; Evaluate externally

(defgeneric quietly-exe (string)
  (:method ((string string))
    #?{ (let* ((*standard-output* (make-broadcast-stream)))
          ${string}) }))

(defun eval-stdin-string-exe ()
  #?{ (loop :for form = (read *standard-input*
                              nil
                              *standard-input*)
            :until (eq form *standard-input*)
            :do (eval form)) })

(defgeneric exec-eval-externally (implementation forms &key no-init output))

(defmethod exec-eval-externally (impl (forms list) &rest rest)
  (let ((input-string (with-output-to-string (*standard-output*)
                        (dolist (form forms)
                          (assert (stringp form))
                          (princ form)
                          (terpri)))))
    (apply #'exec-eval-externally impl input-string rest)))

(defmethod exec-eval-externally ((impl exec-sbcl) (input-string string)
                                 &key no-init (output :string))
  (with-input-from-string (input input-string)
    (uiop:run-program (append (list (binary-path impl))
                              (static-arguments impl)
                              (if no-init
                                  (list "--no-userinit" "--no-sysinit")
                                  ())
                              (list "--eval" (eval-stdin-string-exe))
                              (list "--quit"))
                      :input input
                      :output output
                      :error-output *debug-io*)))

(defmethod exec-eval-externally ((impl exec-ecl) (input-string string)
                                 &key no-init (output :string))
  (with-input-from-string (input input-string)
    (uiop:run-program (append (list (binary-path impl))
                              (static-arguments impl)
                              (if no-init
                                  (list "--norc")
                                  ())
                              (list "--eval" (eval-stdin-string-exe))
                              (list "--eval" "(si:quit)"))
                      :input input
                      :output output
                      :error-output *debug-io*)))

;;; Command line arguments

;; For ECL we can use "--" to indicate where the arguments to the
;; script begin.
(defun ecl-script-arguments-string-exe ()
  #?{(loop :for tail :on (ext:command-args) \
           :when (equal "--" (car tail)) \
           :return (cdr tail))})

(defgeneric exec-command-line-args-string-for-mode (implementation mode)
  (:method ((impl exec-sbcl) mode)
    #?{(cdr sb-ext:*posix-argv*)})
  (:method ((impl exec-ecl) mode)
    ;; #?{(loop for i from 1 below (si:argc) collect (si:argv i))}
    #?{(cdr (ext:command-args))})
  (:method ((impl exec-ecl) (mode exec-lisp-script))
    (ecl-script-arguments-string-exe))
  (:method ((impl exec-ecl) (mode exec-shell-script))
    (ecl-script-arguments-string-exe)))

(defgeneric exec-get-argv-string (implementation)
  (:method ((impl exec-sbcl))
    #?{sb-ext:*posix-argv*})
  (:method ((impl exec-ecl))
    #?{(ext:command-args)}))

;;; Exit

(defgeneric exec-exit-string (implementation &optional code)
  (:method ((impl exec-sbcl) &optional (code 0))
    #?{(sb-ext:exit :code ${code})})
  (:method ((impl exec-ecl) &optional (code 0))
    #?{(si:exit ${code})}))

;;; Disable debugger

(defgeneric exec-disable-debugger-string (implementation)
  (:method ((impl exec-sbcl))
    #?{(sb-ext:disable-debugger)})
  (:method ((impl exec-ecl))
    #?{(setf *debugger-hook* \
             (lambda (condition debugger) \
               (si:exit 1)))}))


