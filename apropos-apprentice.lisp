;;;; Requires
;;;;   slime-apprentice
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

(in-package :slime-apprentice) cx

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

(defmethod apprentice-update ((ap apropos-apprentice)
                              (object symbol))
  (alexandria:when-let* ((fn (ignore-input-function ap))
                         (ignore (funcall fn object)))
    (return-from apprentice-update nil))
  (let* ((package-indicator (get object 'package-indicator))
         (symbol-package (symbol-package object))
         (symbols (make-hash-table))
         (inherited-symbols (make-hash-table))
         (non-inherited-symbols (make-hash-table))
         (interesting-packages)
         (interesting-symbol-count 0))
    (with-output-to-string (*standard-output*)
      (when package-indicator
        (dolist (pkg (packages-with-names-like package-indicator))
          (pushnew pkg interesting-packages)))
      (when symbol-package
        ;; In cases like "some-values" doing apropos on "values" will
        ;; only show symbols from CL, not MY-PACKAGE::SOME-VALUES. The
        ;; following form fixes that.
        (when (member symbol-package (package-use-list *package*))
          (push *package* interesting-packages))
        (dolist (pkg (packages-with-names-like
                      (package-name symbol-package)))
          (pushnew pkg interesting-packages)))
      (let ((interesting-symbol-function
              (interesting-symbol-function ap)))
        (dolist (pkg interesting-packages)
          (dolist (sym (apropos-list object pkg (not (eq pkg *package*))))
            (unless (eq sym object)
              (when (funcall interesting-symbol-function sym)
                (incf interesting-symbol-count)
                (cond ((eql *package* (symbol-package sym))
                       (setf (gethash sym non-inherited-symbols) t))
                      ((eql pkg *package*)
                       (setf (gethash sym inherited-symbols) t))
                      (t (setf (gethash sym symbols) t))))))))
      (when (zerop interesting-symbol-count)
        (return-from apprentice-update nil))
      (fresh-line)
      (princ "Apropos: ")
      (terpri)
      (unless (zerop (hash-table-count non-inherited-symbols))
        (loop :with did-print
              :for key :being :each :hash-key
              :in non-inherited-symbols
              :do (print key)
                  (setf did-print t)
              :finally (when did-print
                         (terpri))))
      (unless (zerop (hash-table-count inherited-symbols))
        (loop :with did-print
              :for key :being :each :hash-key
              :in inherited-symbols
              :do (print key)
                  (setf did-print t)
              :finally (when did-print
                         (terpri))))
      (unless (zerop (hash-table-count symbols))
        (loop :for key :being :each :hash-key
              :in symbols
              :unless (gethash key inherited-symbols)
              :do (print key))))))
