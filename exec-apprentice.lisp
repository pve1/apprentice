;;;; Requires
;;;;   apprentice
;;;;   "buttons"
;;;;   "emacs"
;;;;   "option"
;;;;   "executable"
;;;;   "build-wizard"

(in-package :apprentice) cx

;;; Making executable things

;;; Objective: Make any lisp function available as a lisp script or a
;;; shell command either through a shell script or by saving a core.

(defclass exec-apprentice ()
  ((build-wizard :initarg :build-wizard
                 :accessor build-wizard
                 :initform nil))
  (:default-initargs
   :modes
   (option "Mode"
           `((shell-script exec-shell-script)
             (lisp-script exec-lisp-script)
             (executable 
              exec-executable-core
              (suboptions
               ,(option 
                 "Executable type"
                 '((normal exec-program-core
                    (message "Load system, then save-lisp-and-die."))
                   (slim exec-slim-program-core
                    (message "Only the fasls."))))))))
   :implementations
   (option "Implementation" '((sbcl exec-sbcl)
                              (ecl exec-ecl)))
   :arg-parsing-methods
   (option "Arg parsing"
           `((unparsed
              unparsed
              (message "(defun command (a b &optional c &rest d) ...)"))
             (parsed
              (call-with-args "apprentice::parse-command-line-options-flexible")
              (message "(defun command (a b &key c d) ...) (see command-line.lisp for details)")
              (requires "apprentice/command-line"))
             (argv argv (message "(defun command (&rest argv) ...)"))
             (none none (message "(defun command () ...)"))))
   :output-directory-alternatives
   (option "Output directory"
           `(("./" buffer-dir-exe)
             ("~/bin/" ,(in-homedir-exe "bin/"))
             ( "~/.local/bin/" ,(in-homedir-exe ".local/bin/")))
           :select-value-function (lambda (val)
                                    (typecase val
                                      (symbol (funcall val))
                                      (t val))))
   :output-extension-alternatives
   (option "Output extension" '((yes t)
                                (no nil))))
  (:documentation ""))

(defmethod initialize-instance :after ((ap exec-apprentice) 
                                       &key modes
                                            implementations
                                            arg-parsing-methods
                                            output-extension-alternatives
                                            output-directory-alternatives)
  (let (implementations*)
    ;; Make instances from pathnames like "/usr/bin/sbcl".
    (when (listp implementations)
      (setf implementations*
            (option
             "Implementation"
             (mapcar (lambda (i &aux (impl (coerce-to-implementation-exe i)))
                       (list (name impl) impl))
                     implementations))))
    ;; Setup the wizard.
    (setf (build-wizard ap)
          (make-instance 'build-wizard
            :options
            (list :implementations (or implementations*
                                       implementations)
                  :modes modes                  
                  :arg-parsing-methods arg-parsing-methods
                  :output-extension-alternatives
                  output-extension-alternatives
                  :output-directory-alternatives
                  output-directory-alternatives)))))

(defun in-homedir-exe (pathname)
  (merge-pathnames pathname (user-homedir-pathname)))

(defun buffer-dir-exe ()
  (directory-namestring
   (buffer-context-property :filename)))

(defmethod exec-current-option (apprentice option-name)
  (option-selected-suboption
   (wizard-option (build-wizard apprentice) option-name)))

(defmethod exec-current-option-value (apprentice option-name)
  (option-selected-suboption-value
   (wizard-option (build-wizard apprentice) option-name)))

(defmethod exec-current-option-property (apprentice option-name property)
  (option-selected-property
   (exec-current-option apprentice option-name)
   property))

(defmethod current-implementation (apprentice)
  (exec-current-option-value apprentice :implementations))

(defmethod current-mode (apprentice)
  (exec-current-option-value apprentice :modes))

(defmethod current-arg-parsing-method (apprentice)
  (exec-current-option-value apprentice :arg-parsing-methods))

(defmethod current-output-directory (apprentice)
  (exec-current-option-value apprentice 
                             :output-directory-alternatives))

(defmethod current-output-extension-alternative (apprentice)
  (exec-current-option-value apprentice
                             :output-extension-alternatives))

(defmethod describe-with-apprentice ((ap exec-apprentice)
                                     (object symbol)
                                     stream)
  (executable-apprentice-describe ap object stream))

(defmethod executable-apprentice-describe ((ap exec-apprentice)
                                           object
                                           stream
                                           &key entry-point
                                                file
                                                system)
  (alexandria:when-let* ((file (or file (buffer-context-property
                                         :filename)))
                         (system (or system
                                     (source-file-system-name file))))
    (let ((*standard-output* stream))
      (terpri)
      (wizard-display (build-wizard ap))
      (fresh-line)
      ;; Check that output directory exists.
      (unless (probe-file (current-output-directory ap))
        (format t "~&~%Directory ~S doesn't exist!"
                (current-output-directory ap))
        (return-from executable-apprentice-describe nil))
      (finish-output)
      (let* ((arg-parsing-system
               (alexandria:when-let*
                   ((opt (exec-current-option ap :arg-parsing-methods))
                    (selected (selected opt))
                    (req (option-selected-property opt 'requires)))
                 (check-type req string)
                 req))
             (exec (make-build
                    ap
                    :mode (instantiate-maybe
                           (current-mode ap))
                    :arg-parser (current-arg-parsing-method ap)
                    :required-system system
                    :additional-systems arg-parsing-system
                    :implementation (instantiate-maybe
                                     (current-implementation ap))
                    :entry-point (or entry-point object)
                    :output-directory (current-output-directory ap)
                    :package (package-name (suggested-current-package))
                    :extensionp
                    (current-output-extension-alternative ap))))
        (terpri)
        ;; Display executable.
        (princ (show-build exec))
        (fresh-line)
        (terpri)
        (when exec
          (put-lisp-button-here
           ap "[MAKE-IT-SO]"
           (lambda (x)
             (declare (ignore x))
             (make-it-so exec))))
        t))))
