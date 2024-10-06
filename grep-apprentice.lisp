;;;; Requires
;;;;   slime-apprentice
;;;;   "caching-apprentice"
;;;;   "buttons"

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
            (let ((f (getf *buffer-context* :filename)))
              (when f
                (truename f))))
          (results)
          (offset *standard-output*))

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
        (create-ephemeral-elisp-function
         apprentice 'grep-apprentice-button
         '(lambda (symbol-name filename)
           (progn (find-file-other-window filename)
                  (condition-case
                   nil
                   (progn
                     (search-forward symbol-name)
                     (slime-flash-region
                      (match-beginning 0)
                      (match-end 0)
                      0.5)
                     (end-of-line))
                   (error (beginning-of-buffer)))
                  (other-window 1))))
        (dolist (file files)
          (let ((count (count-matching-lines string file)))
            (unless (zerop count)
              (push (list (enough-namestring file
                                             (or (path apprentice)
                                                 buffer-context-filename
                                                 *default-pathname-defaults*))
                          count
                          (namestring file))
                    results))))
        (fresh-line)
        (princ "Mentions: ")
        (when results
          (setf results (sort results #'> :key #'second))
          (terpri)
          (terpri)
          (dolist (r results)
            (put-elisp-button-here
             apprentice
             (format nil "~A: ~A"
                     (first r)
                     (second r))
             nil
             :name 'grep-apprentice-button
             :arguments (list (symbol-name object)
                              (third r))
             :offset offset)
            (terpri))
          (fresh-line))))))
