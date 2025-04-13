;;;; Requires
;;;;   asdf
;;;;   uiop
;;;;   cl-ppcre
;;;;   eclector
;;;;   "package"

(in-package :apprentice) cx

;;; System dependencies

(defun system-fasl-files (system-name)
  (loop :for c :in (asdf:required-components
                    (asdf:find-system system-name))
        :when (typep c 'asdf:cl-source-file)
        :append (asdf:output-files 'asdf:compile-op c)))

(defun walk-system-dependencies (system-name fn
                                 &optional descend-predicate)
  (setf descend-predicate (or descend-predicate
                              (constantly t)))
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
               resolved-deps))
           (walk (system-name)
             (when (funcall descend-predicate system-name)
               (let* ((deps (dependencies system-name)))
                 (dolist (dep deps)
                   (walk dep))
                 (funcall fn system-name)))))
    (mapc #'walk (dependencies system-name))
    nil))

(defun system-dependencies (system)
  (let ((deps ())
        (seen (make-hash-table :test 'equal)))
    (walk-system-dependencies
     system
     (lambda (x)
       (push x deps))
     (lambda (x)
       (unless (gethash x seen)
         (setf (gethash x seen) t))))
    (nreverse deps)))

(defun regex-system-set (regex)
  (let ((systems ()))
    (dolist (system (asdf:registered-systems))
      (when (cl-ppcre:scan regex system)
        (push system systems)))
    systems))

(defun subsystem-set (system &optional (include-primary-p t))
  "Returns a list containing subsystems of SYSTEM. If INCLUDE-PRIMARY-P
is non-nil the list also includes SYSTEM."
  (regex-system-set
   (concatenate 'string "^"
                (cl-ppcre:quote-meta-chars system)
                (if include-primary-p
                    "/?"
                    "/"))))

(defun list-as-table-sys (list &key (test 'equal) value-fn)
  (let ((table (make-hash-table :test test)))
    (dolist (e list)
      (setf (gethash e table) (if value-fn
                                  (funcall value-fn e)
                                  t)))
    table))

(defun system-set-dependencies (systems)
  "Returns the dependencies of SYSTEMS (a list of system names).

Example:
  (system-set-dependencies
   (subsystem-set \"apprentice\"))

This will return the dependencies of apprentice and its known
subsystems."
  (let ((seen (make-hash-table :test 'equal))
        (original-set (list-as-table-sys systems))
        (deps nil))
    (dolist (system systems)
      (walk-system-dependencies
       system
       (lambda (x)
         (unless (gethash x original-set)
           (push x deps)))
       (lambda (x)
         (unless (gethash x seen)
           (setf (gethash x seen) t)))))
    (nreverse deps)))

(defun fasls-to-load-system (system)
  "Returns a list of fasl files that, when loaded, is equivalent to
calling (asdf:load-system SYSTEM), assuming all fasls are up-to-date.
The second value returned is a list of 'require' systems, such as
sb-posix, that need to be required before loading the fasls."
  (let ((deps (system-dependencies system))
        (require-systems ()))
    ;; Check for 'require' systems.
    (dolist (sysname deps)
      (let ((sys (asdf:find-system sysname)))
        (when (typep sys 'asdf:require-system)
          (push (asdf:component-name sys) require-systems))))
    (values (loop :for sys :in (append deps (list system))
                  :append (system-fasl-files sys))
            require-systems)))

(defun fasls-to-load-multiple-systems (systems)
  (let* ((require-systems ())
         (all-fasls
           (loop :for sys :in systems
                 :append (multiple-value-bind (fasls req-sys)
                             (apprentice::fasls-to-load-system sys)
                           (setf require-systems
                                 (append req-sys require-systems))
                           fasls))))
    (values (remove-duplicates all-fasls
                               :test #'equal
                               :from-end t)
            (remove-duplicates require-systems
                               :test #'string=))))

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

\(determine-subsystem-name \"/myapp/util/macros.lisp\"
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
