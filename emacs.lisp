;;;; Requires
;;;;   alexandria
;;;;   swank
;;;;   "apprentice"
;;;;   "symbols"

;;;; Functions to make Emacs do things.

(in-package :apprentice) cx

(defun eval-in-emacs (form &optional buffer)
  (if buffer
      (swank:eval-in-emacs
       `(with-current-buffer ,buffer
          ,form))
      (swank:eval-in-emacs form)))

(defun eval-in-emacs-current-buffer (form)
  (eval-in-emacs form (buffer-context-property :buffer-name)))

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

;;; Buffer string

(defmethod Emacs-buffer-string (buffer-form &optional begin end)
  "From the Emacs buffer indicated by BUFFER-FORM, return the substring
from the BEGIN to END. The indexes are 1-based, not 0-based, as Emacs
expects."
  (let ((b (if begin
               begin
               '(point-min)))
        (e (if end
               end
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

;;; Defun parameters

(defun partial-defun-string-emc (buffer-form &optional position)
  (eval-in-emacs
   `(with-current-buffer ,buffer-form
      (let ((end (or ,position (point))))
        (save-excursion
         (beginning-of-defun)
         (buffer-substring-no-properties (point) end))))))

(defun partial-defun-emc (buffer &optional position)
  (let* ((substring (partial-defun-string-emc buffer position))
         (forms))
    (when (eql #\( (alexandria:first-elt substring))
      (with-input-from-string (s substring :start 1)
        (ignore-errors
         (loop :for form = (read s nil s)
               :until (eq form s)
               :do (push form forms))))
      (setf forms (nreverse forms)))
    forms))

(defun Emacs-defun-lambda-list (buffer &optional position)
  (let ((defun (partial-defun-emc buffer position)))
    (case (car defun)
      (defun (third defun))
      (defmacro (third defun))
      ;; Skip method qualifiers
      (defmethod (loop :for form :in (nthcdr 2 defun)
                       :when (listp form)
                       :return form)))))

;; Only defun currently.
(defun Emacs-defun-parameters (buffer &optional position)
  (let ((lambda-list (emacs-defun-lambda-list buffer position)))
    (multiple-value-bind (req optional rest kw aok aux keyp)
        (alexandria:parse-ordinary-lambda-list lambda-list)
      (append req
              (mapcar #'car optional)
              (alexandria:ensure-list rest)
              (mapcar #'cadar kw)
              (mapcar #'car aux)))))
