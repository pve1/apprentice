;; -*- lexical-binding: t -*-

(define-derived-mode slime-apprentice-mode text-mode
  "Slime apprentice")

(define-key slime-apprentice-mode-map (kbd "q") 'kill-buffer-and-window)
(define-key slime-apprentice-mode-map (kbd "l") 'slime-apprentice-lock-apprentice)
(define-key slime-apprentice-mode-map (kbd "L") 'slime-apprentice-lock-apprentice-and-split)
(define-key slime-apprentice-mode-map (kbd "g") 'slime-apprentice-update-apprentice-buffer)
(define-key slime-apprentice-mode-map (kbd "f") 'slime-apprentice-faster-polling)
(define-key slime-apprentice-mode-map (kbd "F") 'slime-apprentice-slower-polling)
(define-key slime-apprentice-mode-map (kbd "+") 'slime-apprentice-faster-polling)
(define-key slime-apprentice-mode-map (kbd "-") 'slime-apprentice-slower-polling)
(define-key slime-apprentice-mode-map (kbd "m") 'slime-apprentice-toggle-update-mode)

(defvar slime-apprentice-polling-frequency 0.5)
(defvar slime-apprentice-buffer-name "*slime-apprentice*")
(defvar slime-apprentice-update-mode 'idle) ; or 'continuous
(defvar slime-apprentice-force-update nil)
(defvar slime-apprentice-describe-timer nil)
(defvar slime-apprentice-active-in-strings nil)

;; Valid members are:
;; - toplevel-form
;; - enclosing-form
;; - package
;; - filename
(defvar slime-apprentice-provide-context '())

(defvar-local slime-apprentice-buffer-context nil)
(defvar-local slime-apprentice-variable-name nil)
(defvar-local slime-apprentice-presentation-id nil)
(defvar-local slime-apprentice-form-string nil)
(defvar-local slime-apprentice-package nil)
(defvar-local slime-apprentice-locked-p nil)

(defvar slime-apprentice-help-line
  (let ((s " [q]:quit [l|L]:lock [-|+]:freq [m]:mode ")
        (s2 "\n"))
    (setf s (propertize s 'face 'fringe 'font-lock-face 'fringe))
    (setf s2 (propertize s2 'face 'default 'font-lock-face 'default))
    (concat s s2)))

(defvar slime-apprentice-locked-help-line
  (let ((s " [q]:quit [-]|[+]:freq [m]:mode ")
        (s2 "\n"))
    (setf s (propertize s 'face 'fringe 'font-lock-face 'fringe))
    (setf s2 (propertize s2 'font-lock-face 'default 'font-lock-face 'default))
    (concat s s2)))

(defun slime-apprentice-insert-help-line ()
  (if slime-apprentice-locked-p
      (insert slime-apprentice-locked-help-line)
    (insert slime-apprentice-help-line)))

(defun slime-apprentice-insert (string)
  (slime-apprentice-insert-help-line)
  (insert string))

(defun slime-apprentice-check-apprentice-buffer ()
  (unless (eql major-mode 'slime-apprentice-mode)
    (error "Not an apprentice buffer.")))

(defun slime-apprentice-buffer-p (buf)
  (let ((mode (buffer-local-value 'major-mode buf)))
    (eql mode 'slime-apprentice-mode)))

(defun slime-apprentice-window-p (win)
  (slime-apprentice-buffer-p (window-buffer win)))

(defun slime-apprentice-faster-polling ()
  (interactive)
  (setf slime-apprentice-polling-frequency
        (/ slime-apprentice-polling-frequency 2.0))
  (slime-apprentice-reinitialize-timer)
  (message "%s" slime-apprentice-polling-frequency))

(defun slime-apprentice-slower-polling ()
  (interactive)
  (setf slime-apprentice-polling-frequency
        (* slime-apprentice-polling-frequency 2.0))
  (slime-apprentice-reinitialize-timer)
  (message "%s" slime-apprentice-polling-frequency))

(defun slime-apprentice-set-buffer-input (input &optional buffer context)
  (unless buffer
    (setf buffer (current-buffer)))
  (with-current-buffer buffer
    (slime-apprentice-check-apprentice-buffer)
    (setf slime-apprentice-buffer-context context)
    (cond ((integerp input) ; presentation
           (setf slime-apprentice-presentation-id input)
           (setf slime-apprentice-variable-name nil))
          ((stringp input) ; symbol
           (setf slime-apprentice-variable-name input)
           (setf slime-apprentice-presentation-id nil))
          ((and (listp input) ; form
                (eq 'form (car input)))
           (setf slime-apprentice-variable-name nil)
           (setf slime-apprentice-presentation-id nil)
           (setf slime-apprentice-package (cl-getf input 'package))
           (setf slime-apprentice-form-string (cadr input)))
          (t (error "Bad name or presentation.")))))

(defun slime-apprentice-retrieve-description-for-buffer ()
  (condition-case nil
      (cond (slime-apprentice-presentation-id
             (slime-eval
              `(cl:let ((slime-apprentice::*force-return-description*
                         ,slime-apprentice-force-update)
                        (slime-apprentice::*buffer-context*
                         (cl:quote ,slime-apprentice-buffer-context)))
                       (slime-apprentice:presentation-description
                        ,slime-apprentice-presentation-id))))
            (slime-apprentice-variable-name
             (slime-eval
              `(cl:let ((slime-apprentice::*force-return-description*
                         ,slime-apprentice-force-update)
                        (slime-apprentice::*buffer-context*
                         (cl:quote ,slime-apprentice-buffer-context)))
                       (slime-apprentice:symbol-description
                        ,slime-apprentice-variable-name))))
            (slime-apprentice-form-string
             ;; Ensure the package is fixed this buffer. This should not be needed.
             (unless slime-apprentice-package
               (setf slime-apprentice-package
                     (slime-eval `(cl:package-name cl:*package*))))
             (slime-eval
              `(cl:let ((slime-apprentice::*force-return-description*
                         ,slime-apprentice-force-update)
                        (slime-apprentice::*buffer-context*
                         (cl:quote ,slime-apprentice-buffer-context)))
                       (slime-apprentice:form-description
                        ,slime-apprentice-form-string
                        ,slime-apprentice-package))))
            (t (error "Missing variable or presentation.")))
    (error (slime-apprentice-cancel-timer)
           (setf slime-apprentice-describe-timer nil)
           (message "Error retrieving description. Are we consing yet?")
           nil)))

(defun slime-apprentice-create-apprentice-buffer ()
  (let ((buffer (get-buffer-create slime-apprentice-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'slime-apprentice-mode)
        (slime-apprentice-mode))
      buffer)))

(defun slime-apprentice-update-apprentice-buffer (&optional buffer input)
  (interactive)
  (unless buffer
    (setf buffer (current-buffer)))
  (with-current-buffer buffer
    (slime-apprentice-check-apprentice-buffer)
    (when input
      (slime-apprentice-set-buffer-input input buffer))
    (when (or slime-apprentice-presentation-id
              slime-apprentice-variable-name
              slime-apprentice-form-string)
      (let ((results (slime-apprentice-retrieve-description-for-buffer)))
        (cond ((eql results :unchanged)
               nil)
              ((eql results :max-size-exceeded)
               (erase-buffer)
               (slime-apprentice-insert "[Max size exceeded]"))
              ((stringp results)
               (erase-buffer)
               (slime-apprentice-insert results)))))
    (goto-char (point-min))))

(defun slime-apprentice-update-the-apprentice-buffer (&optional input)
  (interactive)
  (let ((buffer (or (get-buffer slime-apprentice-buffer-name)
                    (slime-apprentice-create-apprentice-buffer))))
    (slime-apprentice-update-apprentice-buffer buffer input)))

(defun slime-apprentice-enclosing-form-position ()
  (condition-case nil
      (save-excursion
        (up-list -1 t)
        (when (eql ?\" (char-after (point)))
          (up-list -1 t))
        (point))
    (error nil)))

(defun slime-apprentice-enclosing-form ()
  (condition-case nil
      (let ((enclosing-form-position
             (slime-apprentice-enclosing-form-position)))
        (save-excursion
          (up-list -1 t)
          (when (eql ?\" (char-after (point)))
            (up-list -1 t))
          (forward-sexp)
          (buffer-substring-no-properties
           enclosing-form-position
           (point))))
    (error nil)))

(defun slime-apprentice-toplevel-form ()
  (condition-case nil
      (save-excursion
        (let (begin)
          (beginning-of-defun)
          (setf begin (point))
          (forward-list)
          (buffer-substring-no-properties
           begin (point))))
    (error nil)))

(defun slime-apprentice-package ()
  (condition-case nil
      (slime-current-package)
    (error nil)))

(defun slime-apprentice-build-context ()
  (when slime-apprentice-provide-context
    `(,@(when (member 'enclosing-form slime-apprentice-provide-context)
          (list :enclosing-form (slime-apprentice-enclosing-form)))
      ,@(when (member 'toplevel-form slime-apprentice-provide-context)
          (list :toplevel-form (slime-apprentice-toplevel-form)))
      ,@(when (member 'package slime-apprentice-provide-context)
          (list :package (slime-apprentice-package)))
      ,@(when (member 'filename slime-apprentice-provide-context)
          (list :filename (buffer-file-name))))))

(defun slime-apprentice-set-input-from-point-maybe ()
  (let ((string (ignore-errors (thing-at-point 'string t)))
        (symbol (thing-at-point 'symbol t))
        (presentation (car (slime-presentation-around-or-before-point
                            (point))))
        (presentation-id))
    (when presentation
      (setf presentation-id (slime-presentation-id presentation)))
    (when (or presentation-id
              (and (or slime-apprentice-active-in-strings
                       (not string))
                   symbol))
      (slime-apprentice-set-buffer-input
       (or presentation-id symbol)
       slime-apprentice-buffer-name
       (slime-apprentice-build-context)))))

(defun slime-apprentice-update-if-live-window (window)
  (let ((buf (window-buffer window)))
    (when (and (slime-apprentice-buffer-p buf)
               (not (eql (current-buffer) buf)))
      (slime-apprentice-update-apprentice-buffer buf))))

(defun slime-apprentice-timer-function ()
  (interactive)
  (when (and (or (eql major-mode 'lisp-mode)
                 (eql major-mode 'slime-repl-mode))
             (get-buffer slime-apprentice-buffer-name))
    (slime-apprentice-set-input-from-point-maybe))
  (walk-windows #'slime-apprentice-update-if-live-window nil t))

(defun slime-apprentice-reinitialize-continuous-timer ()
  (interactive)
  (ignore-errors (cancel-timer slime-apprentice-describe-timer))
  (setf slime-apprentice-update-mode 'continuous)
  (setf slime-apprentice-describe-timer
        (run-with-timer 0
                        slime-apprentice-polling-frequency
                        'slime-apprentice-timer-function))
  (message "Continuous mode."))

(defun slime-apprentice-reinitialize-idle-timer ()
  (interactive)
  (ignore-errors (cancel-timer slime-apprentice-describe-timer))
  (setf slime-apprentice-update-mode 'idle)
  (setf slime-apprentice-describe-timer
        (run-with-idle-timer slime-apprentice-polling-frequency
                             t
                             'slime-apprentice-timer-function))
  (message "Idle mode."))

(defun slime-apprentice-reinitialize-timer ()
  (interactive)
  (ignore-errors (cancel-timer slime-apprentice-describe-timer))
  (if (eql slime-apprentice-update-mode 'idle)
      (slime-apprentice-reinitialize-idle-timer)
    (slime-apprentice-reinitialize-continuous-timer)))

(defun slime-apprentice-toggle-update-mode ()
  (interactive)
  (if (eql slime-apprentice-update-mode 'idle)
      (slime-apprentice-reinitialize-continuous-timer)
    (slime-apprentice-reinitialize-idle-timer)))

(defun slime-apprentice-cancel-timer ()
  (interactive)
  (cancel-timer slime-apprentice-describe-timer))

(defun slime-apprentice-lock-apprentice ()
  (interactive)
  (slime-apprentice-check-apprentice-buffer)
  (let ((buffer-name (format "%s:%s"
                             slime-apprentice-buffer-name
                             (or slime-apprentice-variable-name
                                 slime-apprentice-presentation-id
                                 (and slime-apprentice-form-string
                                      (substring
                                       slime-apprentice-form-string
                                       0 (min 20 (length slime-apprentice-form-string))))))))
    (cond ((get-buffer buffer-name)
           (switch-to-buffer buffer-name))
          (t (rename-buffer buffer-name)
             (setf slime-apprentice-locked-p t)
             (let ((slime-apprentice-force-update t))
               (slime-apprentice-update-apprentice-buffer buffer-name))
             (slime-apprentice-create-apprentice-buffer)))))

(defun slime-apprentice-lock-apprentice-and-split ()
  (interactive)
  (slime-apprentice-lock-apprentice)
  (split-window)
  (switch-to-buffer slime-apprentice-buffer-name))

;; Interactive function
(defun slime-apprentice-describe (prefix)
  (interactive "P")
  (if prefix
      (slime-apprentice-describe-form)
    (progn
      (unless slime-apprentice-describe-timer
        (slime-apprentice-reinitialize-timer))
      (slime-apprentice-create-apprentice-buffer)
      (slime-apprentice-set-input-from-point-maybe)
      (let ((slime-apprentice-force-update t))
        (slime-apprentice-update-the-apprentice-buffer))
      (display-buffer slime-apprentice-buffer-name))))

;; Interactive function
(defun slime-apprentice-describe-form ()
  (interactive)
  (let ((form (save-excursion
                (let (begin end)
                  (backward-sexp)
                  (setf begin (point))
                  (forward-sexp)
                  (setf end (point))
                  (buffer-substring-no-properties begin end)))))
    (unless slime-apprentice-describe-timer
      (slime-apprentice-reinitialize-timer))
    (slime-apprentice-create-apprentice-buffer)
    (slime-apprentice-set-buffer-input
     (list 'form form
           'package (slime-apprentice-package))
     slime-apprentice-buffer-name
     (slime-apprentice-build-context))
    (let ((slime-apprentice-force-update t))
      (slime-apprentice-update-the-apprentice-buffer))
    (display-buffer slime-apprentice-buffer-name)))
