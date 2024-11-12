;;;; Requires
;;;;   apprentice
;;;;   "caching-apprentice"
;;;;   "buttons"

;;;; Grep apprentice

;;; Note: Symlinked directories confuse [REPLACE].

(in-package :apprentice) cx

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

(defun grep-apprentice-walk-lisp-files (fn directory &key recursive)
  (if recursive
      (let ((files (uiop:directory* (merge-pathnames "*.*"
                                                     directory))))
        (dolist (file files)
          (if (uiop:directory-pathname-p file)
              (let ((name (first (last (rest (pathname-directory file))))))
                (unless (alexandria:starts-with-subseq "." name)
                  (grep-apprentice-walk-lisp-files
                   fn file :recursive recursive)))
              (when (equal "lisp" (pathname-type file))
                (funcall fn file)))))
      (let ((files (uiop:directory* (merge-pathnames "*.lisp"
                                                     directory))))
        (dolist (file files)
          (unless (uiop:directory-pathname-p file)
            (when (equal "lisp" (pathname-type file))
              (funcall fn file)))))))

(defmethod apprentice-description-heading ((ap grep-apprentice))
  "Mentions: ")

(defmethod grep-apprentice-query-replace (ap file from to)
  (swank:eval-in-emacs
   `(let ((buf (get-file-buffer ,file)))
      (when buf
        (display-buffer buf)
        (with-current-buffer buf
          (beginning-of-buffer)
          (query-replace ,from ,to))
        t))))

;; Note: Replace won't work on files that haven't been opened by
;; emacs.
(defmethod grep-apprentice-insert-replace-button (ap from files
                                                  &key offset)
  (put-elisp-button-here
   ap "[REPLACE]"
   `(let ((replacement (read-from-minibuffer "Replacement: "
                                             ,from))
          (files ',(reverse (mapcar #'namestring files)))
          (orig-buf (current-buffer)))
      (dolist (file ',(reverse (mapcar #'namestring files)))
        (let ((buf (get-file-buffer file)))
          (when buf
            (switch-to-buffer buf)
            (unwind-protect
                 (progn (setf apprentice-inhibit-update-p t)
                        (beginning-of-buffer)
                        (query-replace
                         ,from
                         replacement nil)
                        t)
              (setf apprentice-inhibit-update-p nil)
              (switch-to-buffer orig-buf)))))
      (when (y-or-n-p (format "Save %s buffers? "
                              (length files)))
        (dolist (file files)
          (with-current-buffer (get-file-buffer file)
            (save-buffer)))))
   :offset offset))

;;; Note: depends on slime-flash-region
(defmethod apprentice-update ((apprentice grep-apprentice)
                              (object symbol))
  (when (<= 2 (length (symbol-name object)))
    (let ((files)
          (string (symbol-name object))
          (buffer-context-filename
            (let ((f (buffer-context-property :filename)))
              (when (and f (probe-file f))
                (truename f))))
          (results)
          (offset *standard-output*))
      (with-output-to-string (*standard-output*)
        (if (recursive apprentice)
            (grep-apprentice-walk-lisp-files
             (lambda (x) (push x files))
             (or (path apprentice)
                 buffer-context-filename
                 ".")
             :recursive t)
            (setf files (directory
                         (if buffer-context-filename
                             (merge-pathnames
                              "*.lisp"
                              buffer-context-filename)
                             "*.lisp"))))
        ;; Clicking jumps to the next match.
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
          (grep-apprentice-insert-replace-button
           apprentice
           (string-downcase (symbol-name object))
           (mapcar #'third results)
           :offset offset)
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
             :offset offset
             :skippable t)
            (terpri))
          (fresh-line))))))
