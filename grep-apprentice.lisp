;;;; Requires
;;;;   apprentice
;;;;   "caching-apprentice"
;;;;   "buttons"

;;;; Grep apprentice

;;; Note: Symlinked directories confuse [REPLACE].

(in-package :apprentice) cx

(defclass Grep-apprentice (caching-apprentice)
  ((path :initarg :path
         :accessor Path
         :initform nil)
   (recursive :initarg :recursive
              :accessor Recursive
              :initform nil)
   (min-length :initarg :min-length
               :accessor min-length
               :initform 2)
   (max-length :initarg :max-length
               :accessor max-length
               :initform 100)
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

(defmethod Apprentice-description-heading ((ap grep-apprentice))
  "Mentions: ")

(defmethod Grep-apprentice-query-replace (ap file from to)
  (eval-in-emacs
   `(let ((buf (get-file-buffer ,file)))
      (when buf
        (display-buffer buf)
        (with-current-buffer buf
          (beginning-of-buffer)
          (query-replace ,from ,to))
        t))))

(defmethod Grep-apprentice-insert-replace-button (ap from files
                                                  &key offset)
  (let ((elisp-form
          `(let ((replacement (read-from-minibuffer "Replacement: "
                                                    ,from))
                 (files ',(reverse (mapcar #'namestring files)))
                 (orig-buf apprentice-buffer-name))
             (dolist (file files)
               (let ((buf (find-file-noselect file)))
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
                   (save-buffer)))))))
    (put-lisp-button-here
     ap "[REPLACE]" `(eval-in-emacs ',elisp-form)
     :offset offset)))

(defmethod apprentice-create-ephemerals ((apprentice grep-apprentice))
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
            (other-window 1)))))

(defmethod apprentice-update :around ((apprentice grep-apprentice)
                                      object)
  ;; Check for region
  (alexandria:when-let*
      ((region (buffer-context-property :region))
       (begin (1- (first region)))
       (end (1- (second region)))
       (ok-size (< (abs (- begin end))
                   (max-length apprentice)))
       (substring (emacs-current-buffer-string begin end))
       (single-line (not (find #\newline substring))))
    ;; Use region
    ;; (format *debug-io* "~&; ~A~%" substring)
    (return-from apprentice-update
      (apprentice-update-with-string apprentice substring)))
  (call-next-method))

;;; Note: depends on slime-flash-region
(defmethod apprentice-update ((apprentice grep-apprentice)
                              (object symbol))
  (apprentice-update-with-string
   apprentice
   (symbol-name object)))

;;; Note: depends on slime-flash-region
(defmethod apprentice-update-with-string ((apprentice grep-apprentice)
                                          (object string))
  (when (<= (min-length apprentice)
            (length object))
    (let* ((files)
           (buffer-context-filename
             (let ((f (buffer-context-property :filename)))
               (when (and f (probe-file f))
                 (truename f))))
           (results)
           (offset *standard-output*))
      ;; Collect files
      (if (recursive apprentice)
          (grep-apprentice-walk-lisp-files
           (lambda (x)
             (push x files))
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
      ;; Setup ephemerals
      (apprentice-create-ephemerals apprentice)
      ;; Perform the grep
      (dolist (file files)
        (let ((count (count-matching-lines object file)))
          (unless (zerop count)
            (push (list (enough-namestring file
                                           (or (path apprentice)
                                               buffer-context-filename
                                               *default-pathname-defaults*))
                        count
                        (namestring file))
                  results))))
      (when results
        (with-output-to-string (*standard-output*)
          ;; Print results
          (fresh-line)
          (princ "Mentions: ")
          (grep-apprentice-insert-replace-button
           apprentice
           (string-downcase object)
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
             :arguments (list object (third r))
             :offset offset
             :skippable t)
            (terpri))
          (fresh-line))))))
