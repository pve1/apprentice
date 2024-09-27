;;;; Requires
;;;;   slime-apprentice
;;;;   "caching-apprentice"

;;;; Grep apprentice

(in-package :slime-apprentice) cx

(defclass Grep-apprentice (caching-apprentice)
  ((path :initarg :path
         :accessor path
         :initform nil)
   (recursive :initarg :recursive
              :accessor recursive
              :initform nil)
   (busy-result :initarg :busy-result
                :accessor busy-result
                :initform "Mentions: ...")))

(defun count-matching-lines (string file)
  (alexandria:with-input-from-file (s file)
    (loop :with count = 0
          :for line = (read-line s nil s)
          :until (eq s line)
          :do (when (search string line :test #'char-equal)
                (incf count))
          :finally (return count))))

(defun grep-apprentice-walk-lisp-files (fn directory)
  (let ((files (uiop:directory* (merge-pathnames "*.*"
                                                 directory))))
    (dolist (file files)
      (if (uiop:directory-pathname-p file)
          (let ((name (first (last (rest (pathname-directory file))))))
            (unless (alexandria:starts-with-subseq "." name)
              (grep-apprentice-walk-lisp-files fn file)))
          (when (equal "lisp" (pathname-type file))
            (funcall fn file))))))

(defmethod apprentice-description-heading ((ap grep-apprentice))
  "Mentions: ")

;;; Note: depends on slime-flash-region
(defmethod apprentice-update ((apprentice grep-apprentice)
                              (object symbol))
  (when (<= 2 (length (symbol-name object)))
    (let ((files)
          (string (symbol-name object))
          (buffer-context-filename
            (getf *buffer-context* :filename))
          (results))
      (with-output-to-string (*standard-output*)
        (if (recursive apprentice)
            (grep-apprentice-walk-lisp-files
             (lambda (x) (push x files))
             (or (path apprentice)
                 buffer-context-filename
                 "."))
            (setf files (directory
                         (if buffer-context-filename
                             (merge-pathnames
                              "*.lisp"
                              buffer-context-filename)
                             "*.lisp"))))
        (dolist (file files)
          (let ((count (count-matching-lines string file)))
            (unless (zerop count)
              (push (list (enough-namestring file (or (path apprentice)
                                                      buffer-context-filename
                                                      *default-pathname-defaults*))
                          count)
                    results))))
        (fresh-line)
        (princ "Mentions: ")
        (when results
          (setf results (sort results #'> :key #'second))
          (terpri)
          (terpri)
          (format t "~:{~A: ~A~%~}" results)
          (fresh-line))))))
