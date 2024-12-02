;;;; Requires
;;;;   "symbols"

;;;; Functions to make Emacs do things.

(in-package :apprentice) cx

(defun eval-in-emacs (form)
  (swank:eval-in-emacs form))

(defun process-form-for-emacs (form)
  (swank::process-form-for-emacs form))

(defun Emacs-message (object)
  (eval-in-emacs `(message ,(princ-to-string object))))

(defmethod Emacs-buffer-string (buffer-form &optional begin end)
  (let ((b (if begin
               (1+ begin)
               '(point-min)))
        (e (if end
               (1+ end)
               '(point-max))))
    (eval-in-emacs
     `(with-current-buffer ,buffer-form
        (buffer-substring-no-properties ,b ,e)))))

(defmethod emacs-buffer-string ((buffer-file pathname)
                                &optional begin end)
  (emacs-buffer-string `(get-file-buffer
                         ,(namestring buffer-file))
                       begin end))

(defmethod emacs-buffer-string ((buffer-name string)
                                &optional begin end)
  (call-next-method))

;; Works in apprentice context
(defun Emacs-current-buffer-string (&optional begin end)
  (let ((bufname (buffer-context-property :buffer-name)))
    (assert bufname)
    (emacs-buffer-string bufname begin end)))

;; An edit can be (1234 :insert "foo:") or (1234 :delete 3).

(defun Emacs-perform-edits (file edits &optional (reindent t))
  (eval-in-emacs
   `(cl-flet
     ((del (n) (delete-forward-char n))
      (ins (str) (insert str)))
     (save-excursion
      (with-current-buffer (find-file-noselect ,file)
        (dolist (edit ',edits)
          (cl-destructuring-bind
           (pos operation &rest args) edit
           (goto-char (1+ pos))
           (cl-case operation
                    (:insert (apply #'ins args))
                    (:delete (apply #'del args)))))
        ,(when reindent
           `(indent-region (point-min) (point-max))))))))

(defun emacs-insert-strings (file position-string-pairs
                             &optional (reindent t))
  (alexandria:when-let
      ((ops (mapcar (lambda (x)
                      (destructuring-bind (pos string) x
                        (list pos :insert string)))
                    position-string-pairs)))
    (emacs-perform-edits file ops reindent)))
