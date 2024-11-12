;;;; Requires
;;;;   apprentice
;;;;   "caching-apprentice"

;;;; Apropos apprentice

;; We need to consider which packages are considered "interesting"
;; when listing apropos and whether to list external or internal
;; symbols.

;; 1. If a package prefix is specified, use packages-with-names-like
;; to find interesting packages and show external symbols. Also try to
;; handle the case where no symbol is specified ("a:")

;; 2. If a package prefix is not specified, use
;; apropos-apprentice-used-packages to find packages mentioned in the
;; current file to show external symbols. Also show internal apropos
;; of *package*.

(in-package :apprentice) cx

(defclass Apropos-apprentice (caching-apprentice)
  ((busy-result :initarg :busy-result
                :accessor busy-result
                :initform "Apropos: ...")
   (interesting-symbol-function
    :initarg :interesting-symbol-function
    :accessor interesting-symbol-function
    :initform (constantly t))
   (ignore-input-function
    :initarg :ignore-input-function
    :accessor ignore-input-function
    :initform (constantly nil))))

(defmethod initialize-instance :after ((a apropos-apprentice)
                                       &key min-length)
  (when min-length
    (check-type min-length integer)
    (setf (ignore-input-function a)
          (lambda (x)
            (< (length (symbol-name x)) min-length)))))

(defmethod eclector.reader:interpret-symbol ((client (eql 'collect-packages))
                                             input-stream
                                             package-indicator
                                             symbol-name
                                             internp)
  (declare (special packages))
  (unless (member package-indicator '(:current :keyword))
    (setf (gethash package-indicator packages) t)))

(defun used-packages-in-file (file)
  (let ((packages (make-hash-table :test 'equal)))
    (declare (special packages))
    (alexandria:with-input-from-file (f file)
      (with-eclector-client 'collect-packages
        (loop :for form = (eclector.reader:read f nil f)
              :until (eq form f))))
    (alexandria:hash-table-keys packages)))

(defun packages-with-names-like (string)
  (loop :for p :in (list-all-packages)
        :when (search string (package-name p))
        :collect p))

(defmethod apropos-compute-interesting-symbols ((ap apropos-apprentice)
                                                (input-symbol symbol))
  (let* ((package-indicator-2 (getf *buffer-context*
                                    'package-indicator))
         (other-symbols (make-hash-table))
         (inherited-symbols (make-hash-table))
         (present-symbols (make-hash-table))
         (interesting-packages)
         (interesting-symbol-count 0))
    (cond ((eq package-indicator-2 :current) ; no package prefix
           (push *package* interesting-packages))
          ((eq package-indicator-2 :keyword)
           (push (find-package :keyword) interesting-packages))
          (t (dolist (pkg (packages-with-names-like
                           package-indicator-2))
               (pushnew pkg interesting-packages))))
    (let ((interesting-symbol-function
            (interesting-symbol-function ap)))
      (dolist (pkg interesting-packages)
        (dolist (sym (apropos-list input-symbol pkg))
          (when (funcall interesting-symbol-function sym)
            (multiple-value-bind (symbol status)
                (find-symbol (symbol-name sym) pkg)
              (when (or (eq pkg *package*)
                        (eq status :external))
                (incf interesting-symbol-count)
                (cond ((and (eql pkg *package*)
                            (member status '(:external :internal)))
                       (setf (gethash sym present-symbols) t))
                      ((eql pkg *package*)
                       (setf (gethash sym inherited-symbols) t))
                      (t (setf (gethash sym other-symbols) t))))))))
      (unless (zerop interesting-symbol-count)
        (list :present (alexandria:hash-table-keys present-symbols)
              :inherited (alexandria:hash-table-keys inherited-symbols)
              :other (alexandria:hash-table-keys other-symbols))))))

(defmethod apropos-export-symbols ((ap apropos-apprentice)
                                   root-symbol
                                   &key breadth
                                        (package *package*))
  (assert (member breadth '(nil :present :all)))
  (destructuring-bind (&key present
                            inherited
                            other)
      (apropos-compute-interesting-symbols ap root-symbol)
    (ecase breadth
      ((:present nil)
       (export present package)
       present))))

(defmethod apropos-export-button ((ap apropos-apprentice) symbol
                                  &key offset)
  (put-lisp-button-here
   ap "[EXPRSNT]"
   `(let ((symbols (apropos-export-symbols
                    *button-apprentice*
                    ',symbol)))
      (emacs-message
       (format nil "Exported ~A symbols from ~S."
               (length symbols)
               (package-name *package*))))
   :offset offset))

(defmethod apropos-import-symbols ((ap apropos-apprentice)
                                   root-symbol
                                   &key (package *package*))
  (destructuring-bind (&key present
                            inherited
                            other)
      (apropos-compute-interesting-symbols ap root-symbol)
    (when other
      (import other package)
      other)))

(defmethod apropos-import-button ((ap apropos-apprentice) symbol
                                  &key offset)
  (put-lisp-button-here
   ap "[IMPORT]"
   `(let ((symbols (apropos-import-symbols
                    *button-apprentice*
                    ',symbol)))
      (emacs-message
       (format nil "Imported ~A symbols into ~S."
               (length symbols)
               (package-name *package*))))
   :offset offset))

(defmethod apprentice-update ((ap apropos-apprentice)
                              (object symbol))
  (alexandria:when-let* ((fn (ignore-input-function ap))
                         (ignore (funcall fn object)))
    (return-from apprentice-update nil))
  (let* ((interesting-symbols
           (apropos-compute-interesting-symbols ap object))
         (offset (file-position *standard-output*)))
    (when interesting-symbols
      (with-output-to-string (*standard-output*)
        (princ "Apropos: ")
        (when (getf interesting-symbols :present)
          (apropos-export-button ap object :offset offset))
        (when (getf interesting-symbols :other)
          (princ " ")
          (apropos-import-button ap object :offset offset))
        (terpri)
        (destructuring-bind (&key inherited present other)
            interesting-symbols
          (let ((*package* (find-package :keyword)))
            (dolist (symbol present)
              (print symbol))
            (dolist (symbol inherited)
              (print symbol))
            (dolist (symbol other)
              (print symbol))))))))
