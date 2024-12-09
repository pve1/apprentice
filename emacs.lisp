;;;; Requires
;;;;   "symbols"

;;;; Functions to make Emacs do things.

(in-package :apprentice) cx

(defun eval-in-emacs (form)
  (swank:eval-in-emacs form))

(defun process-form-for-emacs (form)
  (swank::process-form-for-emacs form))

(defun as-buffer-form-emc (object &key find-file)
  (etypecase object
    (pathname (if find-file
                  `(get-file-buffer ,(namestring object))
                  `(find-file-noselect ,(namestring object))))
    (string object)
    (cons
     (unless (member (car object) '(get-file-buffer
                                    find-file-noselect))
       (warn "Strange buffer form ~S." object))
     object)))

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

(defun Emacs-file-buffer-string (filename &optional begin end)
  (emacs-buffer-string (pathname filename) begin end))

;; Works in apprentice context
(defun Emacs-current-buffer-string (&optional begin end)
  (let ((bufname (buffer-context-property :buffer-name)))
    (assert bufname)
    (emacs-buffer-string bufname begin end)))

;; An edit can be (1234 :insert "foo:") or (1234 :delete 3).

(defun Emacs-perform-edits (buffer edits &optional (reindent t))
  (let ((buffer-form (as-buffer-form-emc buffer)))
    (eval-in-emacs
     `(cl-flet
       ((del (n) (delete-forward-char n))
        (ins (str) (insert str)))
       (with-current-buffer ,buffer-form
         (save-excursion
          (dolist (edit ',edits)
            (cl-destructuring-bind
             (pos operation &rest args) edit
             (goto-char (1+ pos))
             (cl-case operation
                      (:insert (apply #'ins args))
                      (:delete (apply #'del args)))))
          ,(when reindent
             `(indent-region (point-min) (point-max)))))))))

(defun emacs-insert-strings (file position-string-pairs
                             &optional (reindent t))
  (alexandria:when-let
      ((ops (mapcar (lambda (x)
                      (destructuring-bind (pos string) x
                        (list pos :insert string)))
                    position-string-pairs)))
    (emacs-perform-edits file ops reindent)))