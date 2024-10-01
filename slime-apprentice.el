;; -*- lexical-binding: t -*-
(define-derived-mode slime-apprentice-mode text-mode
  "Slime apprentice")

(defvar slime-apprentice-lisp-font-lock-defaults
  `((lisp-cl-font-lock-keywords
     lisp-cl-font-lock-keywords-1
     lisp-cl-font-lock-keywords-2)
    nil
    t
    nil
    nil
	(font-lock-mark-block-function . mark-defun)
    (font-lock-extra-managed-props help-echo)
    (font-lock-syntactic-face-function
     . lisp-font-lock-syntactic-face-function)))

(defun slime-apprentice-fontify-region-using-temp-buffer (b e)
  (interactive "r")
  (let* ((text (buffer-substring-no-properties b e))
         (font-locked (with-temp-buffer
                        (delay-mode-hooks (lisp-mode))
                        (font-lock-mode 1)
                        (insert text)
                        (font-lock-fontify-buffer)
                        (buffer-string))))
    (delete-region b e)
    (goto-char b)
    (insert font-locked)))

;; Simpler, but fontification isn't complete. Keywords are not
;; colored, and only "eclector" in eclector.reader:check-symbol-token.

(defun slime-apprentice-fontify-lisp-region (property)
  (cl-destructuring-bind (tag begin end) property
    (let* ((font-lock-defaults
            slime-apprentice-lisp-font-lock-defaults))
      (remove-list-of-text-properties
       begin end '(face font-lock-face))
      (font-lock-fontify-region begin end))))

(defun slime-apprentice-fontify-lisp-region (property)
  (cl-destructuring-bind (tag begin end) property
    (slime-apprentice-fontify-region-using-temp-buffer begin end)))

(define-key slime-apprentice-mode-map (kbd "q") 'kill-buffer-and-window)
(define-key slime-apprentice-mode-map (kbd "l") 'slime-apprentice-lock-apprentice)
(define-key slime-apprentice-mode-map (kbd "L") 'slime-apprentice-lock-apprentice-and-split)
(define-key slime-apprentice-mode-map (kbd "g") 'slime-apprentice-update-apprentice-buffer)
(define-key slime-apprentice-mode-map (kbd "f") 'slime-apprentice-faster-polling)
(define-key slime-apprentice-mode-map (kbd "F") 'slime-apprentice-slower-polling)
(define-key slime-apprentice-mode-map (kbd "+") 'slime-apprentice-faster-polling)
(define-key slime-apprentice-mode-map (kbd "-") 'slime-apprentice-slower-polling)
(define-key slime-apprentice-mode-map (kbd "m") 'slime-apprentice-toggle-update-mode)
(define-key slime-apprentice-mode-map [?	] 'slime-apprentice-next-button) ; tab
(define-key slime-apprentice-mode-map
  (kbd "<backtab>") 'slime-apprentice-previous-button) ; s-tab

(defface slime-apprentice-dim-face `((t :foreground "Gray50" :weight normal))
  "Dim color")
(defface slime-apprentice-divider `((t :foreground "Chocolate1"))
  "Divider color")

(defvar slime-apprentice-polling-frequency 0.5)
(defvar slime-apprentice-buffer-name "*slime-apprentice*")
(defvar slime-apprentice-update-mode 'idle) ; or 'continuous
(defvar slime-apprentice-force-update nil)
(defvar slime-apprentice-describe-timer nil)
(defvar slime-apprentice-active-in-strings nil)
(defvar slime-apprentice-update-apprentice-buffer-hook
  nil)

;; Valid members are:
;; - toplevel-form
;; - enclosing-form
;; - package
;; - filename
;; - point
;; - current-line
;; - max-line
(defvar slime-apprentice-provide-context '())

(defvar-local slime-apprentice-input nil)
(defvar-local slime-apprentice-buffer-context nil)
(defvar-local slime-apprentice-variable-name nil)
(defvar-local slime-apprentice-presentation-id nil)
(defvar-local slime-apprentice-form-string nil)
(defvar-local slime-apprentice-package nil)
(defvar-local slime-apprentice-looking-at nil)
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

;; "Append"?
(defun slime-apprentice-insert (string &optional properties)
  (let ((offset (point-max)))
    (insert string)
    (when properties
      (dolist (prop properties)
        (cl-case (car prop)
          (slime-apprentice::lisp-button
           (slime-apprentice-insert-lisp-button prop))
          (slime-apprentice::elisp-button
           (slime-apprentice-insert-elisp-button prop))
          (slime-apprentice::fontify-region
           (slime-apprentice-fontify-lisp-region prop))
          (t (error "Bad property %s" prop)))))
    (goto-char (point-min))
    ;; Insert the help line last, so that the property offsets work
    ;; directly. Consider: offsets could be relative.
    (slime-apprentice-insert-help-line)))

(defun slime-apprentice-insert-elisp-button (prop)
  (cl-destructuring-bind (tag begin end label when-clicked-form
                              &optional face redisplay)
      prop
    (setf face (or face 'font-lock-type-face))
    (let ((keymap (make-sparse-keymap))
          (buf (current-buffer))) ; apprentice buffer
      (define-key keymap (kbd "RET")
        (lambda ()
          (interactive)
          (let ((form (car (read-from-string when-clicked-form))))
            (message "%S" form)
            (eval form)
            (when redisplay
              (slime-apprentice-update-apprentice-buffer buf)))))
      ;; Begin and end are zero-based, so we add one.
      (put-text-property (1+ begin) (1+ end) 'keymap keymap)
      (unless (eq :unspecified face)
        (put-text-property (1+ begin) (1+ end)
                           (if font-lock-mode
                               'font-lock-face
                             'face)
                           face))
      (put-text-property (1+ begin) (1+ end)
                         'slime-apprentice-button t))))

(defun slime-apprentice-insert-lisp-button (prop)
  (cl-destructuring-bind (tag begin end label when-clicked-form
                              &optional face redisplay)
      prop
    (setf face (or face 'font-lock-keyword-face))
    (let ((keymap (make-sparse-keymap))
          (buf (current-buffer))) ; apprentice buffer
      (define-key keymap (kbd "RET")
        (lambda ()
          (interactive)
          (message "%s" when-clicked-form)
          (if (stringp when-clicked-form)
              (slime-eval (progn `(cl:eval
                                   (cl:read-from-string
                                    ,when-clicked-form))
                                 t)) ; may otherwise return unreadable
                                     ; objects
            (slime-eval when-clicked-form))
          (when redisplay
            (slime-apprentice-update-apprentice-buffer buf))))
      ;; Begin and end are zero-based, so we add one.
      (put-text-property (1+ begin) (1+ end) 'keymap keymap)
      (unless (eq :unspecified face)
        (put-text-property (1+ begin) (1+ end)
                           (if font-lock-mode
                               'font-lock-face
                             'face)
                           face))
      (put-text-property (1+ begin) (1+ end)
                         'slime-apprentice-button t))))

(defun slime-apprentice-next-button ()
  (interactive)
  (let ((original-pos (point)))
    ;; If point on button, move out.
    (when (and (not (= (point) (point-max)))
               (text-property-any (point)
                                  (1+ (point))
                                  'slime-apprentice-button t))
      (goto-char (next-single-char-property-change
                  (point)
                  'slime-apprentice-button)))
    ;; No button in sight?
    (when (= (point-max)
             (next-single-char-property-change
              (point)
              'slime-apprentice-button))
      (goto-char (point-min)))
    (let ((button (next-single-char-property-change
                   (point)
                   'slime-apprentice-button)))
      (if (= (point-max) button)
          (goto-char original-pos)
        (goto-char button)))))

(defun slime-apprentice-previous-button ()
  (interactive)
  (let ((original-pos (point)))
    ;; If point on button, move out (backwards).
    (when (and (not (= (point) (point-max)))
               (text-property-any (point)
                                  (1+ (point))
                                  'slime-apprentice-button t))
      (goto-char (previous-single-char-property-change
                  (point)
                  'slime-apprentice-button)))
    ;; No button in sight?
    (when (= (point-min)
             (previous-single-char-property-change
              (point)
              'slime-apprentice-button))
      (goto-char (point-max)))
    (let ((button (previous-single-char-property-change
                   (point)
                   'slime-apprentice-button)))
      (if (= (point-min) button)
          (goto-char original-pos)
        (goto-char button)))))

;; (defun slime-apprentice-previous-button ()
;;   (interactive)
;;   (let ((n 0))
;;     (when (= (point) (point-min))
;;       (goto-char (point-max)))
;;     (when (and (not (= (point) (point-max)))
;;                (text-property-any (point)
;;                                   (1+ (point))
;;                                   'slime-apprentice-button t))
;;       (goto-char (previous-single-char-property-change
;;                   (point)
;;                   'slime-apprentice-button))
;;       (cl-incf n))
;;     (condition-case nil
;;         (progn
;;           (goto-char (previous-single-char-property-change
;;                       (point) 'slime-apprentice-button))
;;           (cl-incf n)
;;           (when (< n 2)
;;             (goto-char (previous-single-char-property-change
;;                         (point) 'slime-apprentice-button))))
;;       (error (goto-char (point-max))))))

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

;;; Input should be:

;; thing at point (symbol name, number, presentation)
;; character at point

;; Context can contain:

;; package
;; filename
;; point
;; current-line
;; max-line

(defun slime-apprentice-clear-buffer-input (buffer)
  (with-current-buffer buffer
    (setf slime-apprentice-presentation-id nil)
    (setf slime-apprentice-variable-name nil)
    (setf slime-apprentice-package nil)
    (setf slime-apprentice-form-string nil)
    (setf slime-apprentice-looking-at nil)))

(defun slime-apprentice-check-input (input)
  (unless (and (listp input)
               (symbolp (car input)))
    (error "Invalid input")))

(defun slime-apprentice-set-buffer-input (input &optional buffer context)
  (unless buffer
    (setf buffer (current-buffer)))
  (with-current-buffer buffer
    (slime-apprentice-check-apprentice-buffer)
    (setf slime-apprentice-buffer-context context)
    (slime-apprentice-check-input input)
    (setf slime-apprentice-input input)))

(defun slime-apprentice-retrieve-description-for-buffer ()
  (let* ((context (cdr slime-apprentice-input))
         (eval-form
          (cl-case (car slime-apprentice-input)
            (presentation
             `(slime-apprentice:presentation-description
               ,(slime-apprentice-input-property :id)))
            (symbol
             `(slime-apprentice:symbol-description
               ,(slime-apprentice-input-property :name)))
            (looking-at
             `(slime-apprentice:character-description
               ,(slime-apprentice-input-property :preceding-char)
               ,(slime-apprentice-input-property :following-char))))))
    (unless eval-form
      (error "Missing variable or presentation."))
    (condition-case nil
        (slime-eval
         `(cl:let ((slime-apprentice::*force-return-description*
                    ,slime-apprentice-force-update)
                   (slime-apprentice::*buffer-context*
                    (cl:quote ,context)))
                  ,eval-form))
      (error (slime-apprentice-cancel-timer)
             (setf slime-apprentice-describe-timer nil)
             (message "Error retrieving description. Are we consing yet?")
             nil))))


            ;; (slime-apprentice-form-string
            ;;  ;; Ensure the package is fixed this buffer. This should not be needed.
            ;;  (unless slime-apprentice-package
            ;;    (setf slime-apprentice-package
            ;;          (slime-eval `(cl:package-name cl:*package*))))
            ;;  (slime-eval
            ;;   `(cl:let ((slime-apprentice::*force-return-description*
            ;;              ,slime-apprentice-force-update)
            ;;             (slime-apprentice::*buffer-context*
            ;;              (cl:quote ,slime-apprentice-buffer-context)))
            ;;            (slime-apprentice:form-description
            ;;             ,slime-apprentice-form-string
            ;;             ,slime-apprentice-package))))

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
    (when slime-apprentice-input
      (let ((results (slime-apprentice-retrieve-description-for-buffer)))
        ;; (message "%S" results)
        (cond ((eql results :unchanged)
               nil)
              ((eql results :max-size-exceeded)
               (erase-buffer)
               (slime-apprentice-insert "[Max size exceeded]"))
              ((stringp results)
               (erase-buffer)
               (slime-apprentice-insert results))
              ((listp results)          ; with properties
               (erase-buffer)
               (slime-apprentice-insert (car results) (cadr results))))))
    (goto-char (point-min))
    (run-hooks 'slime-apprentice-update-apprentice-buffer-hook)))

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
    `(,@(when (member 'point slime-apprentice-provide-context)
          (list :point (point)))
      ,@(when (member 'current-line slime-apprentice-provide-context)
          (list :current-line (line-number-at-pos)))
      ,@(when (member 'max-line slime-apprentice-provide-context)
          (list :max-line (line-number-at-pos (point-max))))
      ,@(when (member 'enclosing-form slime-apprentice-provide-context)
          (list :enclosing-form (slime-apprentice-enclosing-form)))
      ,@(when (member 'toplevel-form slime-apprentice-provide-context)
          (list :toplevel-form (slime-apprentice-toplevel-form)))
      ,@(when (member 'package slime-apprentice-provide-context)
          (list :package (slime-apprentice-package)))
      ,@(when (member 'filename slime-apprentice-provide-context)
          (list :filename (buffer-file-name))))))

(defun slime-apprentice-determine-input-at-point ()
  (cl-block determine-input
    ;; Presentation?
    (let* ((presentation
            (car (slime-presentation-around-or-before-point
                  (point))))
           (presentation-id (when presentation
                              (slime-presentation-id presentation))))
      (when presentation-id
        (cl-return-from determine-input
          `(presentation :id presentation-id
                         ,@(slime-apprentice-build-context)))))
    ;; Symbol?
    (let* ((symbol (thing-at-point 'symbol t)))
      (when symbol
        (cl-return-from determine-input
          `(symbol :name ,symbol ,@(slime-apprentice-build-context)))))
    ;; Character
    (let* ((following-char
            (let ((c (following-char)))
              (if (zerop c)
                  nil
                (char-to-string c))))
           (preceding-char
            (let ((c (preceding-char)))
              (if (zerop c)
                  nil
                (char-to-string c)))))
      (cl-return-from determine-input
        `(looking-at :following-char ,following-char
                     :preceding-char ,preceding-char
                     ,@(slime-apprentice-build-context))))))

(defun slime-apprentice-set-input-from-point-maybe ()
  (let ((input (slime-apprentice-determine-input-at-point)))
    (slime-apprentice-set-buffer-input
     input
     slime-apprentice-buffer-name)))

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
    (slime-apprentice-set-input-from-point-maybe)
    (walk-windows #'slime-apprentice-update-if-live-window
                  nil t)))

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

(defun slime-apprentice-input-property (property-name)
  (cl-getf (cdr slime-apprentice-input) property-name))

(defun slime-apprentice-lock-apprentice ()
  (interactive)
  (slime-apprentice-check-apprentice-buffer)
  (let ((buffer-name
         (format "%s:%s"
                 slime-apprentice-buffer-name
                 (cl-case (car slime-apprentice-input)
                   (symbol (slime-apprentice-input-property :name))
                   (presentation (slime-apprentice-input-property :id))
                   (looking-at (slime-apprentice-input-property
                                :following-char))
                   (form (let ((string (slime-apprentice-input-property
                                        :form-string)))
                           (when string
                             (substring string 0 (min 20 (length string))))))))))
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
     (list 'form
           :string form
           :package (slime-apprentice-package))
     slime-apprentice-buffer-name
     (slime-apprentice-build-context))
    (let ((slime-apprentice-force-update t))
      (slime-apprentice-update-the-apprentice-buffer))
    (display-buffer slime-apprentice-buffer-name)))
