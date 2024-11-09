;; -*- lexical-binding: t -*-
(define-derived-mode apprentice-mode text-mode
  "Slime apprentice")

;; Suggested key for apprentice-describe.
;; (define-key lisp-mode-map (kbd "C-c z") (quote apprentice-describe))

(define-key apprentice-mode-map (kbd "q") 'kill-buffer-and-window)
(define-key apprentice-mode-map (kbd "l") 'apprentice-lock-apprentice)
(define-key apprentice-mode-map (kbd "L") 'apprentice-lock-apprentice-and-split)
(define-key apprentice-mode-map (kbd "g") 'apprentice-update-apprentice-buffer)
(define-key apprentice-mode-map (kbd "f") 'apprentice-faster-polling)
(define-key apprentice-mode-map (kbd "F") 'apprentice-slower-polling)
(define-key apprentice-mode-map (kbd "+") 'apprentice-faster-polling)
(define-key apprentice-mode-map (kbd "-") 'apprentice-slower-polling)
(define-key apprentice-mode-map (kbd "m") 'apprentice-toggle-update-mode)
(define-key apprentice-mode-map (kbd "e") 'apprentice-toggle-eager)
(define-key apprentice-mode-map (kbd "M-.") 'slime-edit-definition)
(define-key apprentice-mode-map [?	] 'apprentice-next-button) ; tab
(define-key apprentice-mode-map
  (kbd "<backtab>") 'apprentice-previous-button) ; s-tab
(define-key apprentice-mode-map
  (kbd "C-<return>") 'apprentice-activate-button-then-next)

(defvar apprentice-polling-frequency 0.5)
(defvar apprentice-buffer-name "*apprentice*")
(defvar apprentice-update-mode 'idle) ; or 'continuous
(defvar apprentice-force-update nil)
(defvar apprentice-describe-timer nil)
(defvar apprentice-active-in-strings nil)
(defvar apprentice-eager-p nil)
(defvar apprentice-update-apprentice-buffer-hook
  nil)
(defvar apprentice-inhibit-update-p nil)

;; Valid members are:
;; - point
;; - column
;; - region
;; - line
;; - package
;; - filename
;; - locked
;; - max-line
;; - enclosing-form
;; - toplevel-form

;; Reasonable default
(defvar apprentice-provide-context '(filename
                                     point
                                     column
                                     line
                                     region
                                     package
                                     locked))

(defvar-local apprentice-input nil)
(defvar-local apprentice-buffer-context nil)
(defvar-local apprentice-variable-name nil)
(defvar-local apprentice-presentation-id nil)
(defvar-local apprentice-form-string nil)
(defvar-local apprentice-package nil)
(defvar-local apprentice-looking-at nil)
(defvar-local apprentice-locked-p nil)
(defvar-local apprentice-ephemeral-functions nil)

(defvar apprentice-help-line
  (let ((s "  [q]:quit [l|L]:lock [-|+]:freq [e]:eager [m]:mode ")
        (s2 "\n"))
    (setf s (propertize s 'face 'fringe 'font-lock-face 'fringe))
    (setf s2 (propertize s2 'face 'default 'font-lock-face 'default))
    (concat s s2)))

(defvar apprentice-locked-help-line
  (let ((s "  [q]:quit [-]|[+]:freq [m]:mode ")
        (s2 "\n"))
    (setf s (propertize s 'face 'fringe 'font-lock-face 'fringe))
    (setf s2 (propertize s2 'font-lock-face 'default 'font-lock-face 'default))
    (concat s s2)))

(defface apprentice-dim-face `((t :foreground "Gray50" :weight normal))
  "Dim color")
(defface apprentice-divider `((t :foreground "Chocolate1"))
  "Divider color")

(defvar apprentice-lisp-font-lock-defaults
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

;;; Toggles

(defun apprentice-toggle-eager ()
  (interactive)
  (message "Eager: %s"
           (setf apprentice-eager-p
                 (not apprentice-eager-p))))

;;; Fontification

(defun apprentice-fontify-region-using-temp-buffer (b e)
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

(defun apprentice-fontify-lisp-region (property)
  (cl-destructuring-bind (tag begin end) property
    (let* ((font-lock-defaults
            apprentice-lisp-font-lock-defaults))
      (remove-list-of-text-properties
       begin end '(face font-lock-face))
      (font-lock-fontify-region begin end))))

(defun apprentice-fontify-lisp-region (property)
  (cl-destructuring-bind (tag begin end) property
    (apprentice-fontify-region-using-temp-buffer begin end)))

(defun apprentice-add-face (property)
  (cl-destructuring-bind (tag begin end face) property
    (when (stringp face)
      (setf face (intern face)))
    (put-text-property (1+ begin) (1+ end) 'face face)
    (put-text-property (1+ begin) (1+ end) 'font-lock-face face)))

(defun apprentice-indent-region-using-temp-buffer (b e)
  (interactive "r")
  (let* ((text (buffer-substring b e))
         (formatted (with-temp-buffer
                      (delay-mode-hooks (lisp-mode))
                      (font-lock-mode 1)
                      (insert text)
                      (font-lock-fontify-buffer)
                      (indent-region 1 (point-max))
                      (buffer-string))))
    (delete-region b e)
    (goto-char b)
    (insert formatted)))

(defun apprentice-indent-region (property)
  (cl-destructuring-bind (tag begin end) property
    (apprentice-indent-region-using-temp-buffer begin end)))

(defun apprentice-elisp-ephemeral-callback (function-name
                                            arguments
                                            buf
                                            redisplay
                                            button-order)
  (apply #'apprentice-call-ephemeral function-name arguments)
  (when redisplay
    (apprentice-update-apprentice-buffer buf)
    (when button-order
      (apprentice-jump-to-nth-button button-order))))

(defun apprentice-elisp-callback (when-clicked-form
                                  buf
                                  redisplay
                                  button-order)
  (let ((form (car (read-from-string when-clicked-form))))
    ;; (message "%S" form)
    (eval form)
    (when redisplay
      (apprentice-update-apprentice-buffer buf)
      (when button-order
        (apprentice-jump-to-nth-button button-order)))))

(defun apprentice-activate-elisp-button ()
  (interactive)
  (let ((prop (get-text-property
               (point)
               'apprentice-button-properties))
        (additional (get-text-property
                     (point)
                     'apprentice-button-additional-properties)))
    (when prop
      (cl-destructuring-bind (tag begin end label when-clicked-form
                                  &key face redisplay name
                                  arguments &allow-other-keys)
          prop
        (condition-case-unless-debug error
            (if name
                (apprentice-elisp-ephemeral-callback
                 name
                 arguments
                 (cl-getf additional 'buf)
                 redisplay
                 (cl-getf additional 'button-order))
              (apprentice-elisp-callback
               when-clicked-form
               (cl-getf additional 'buf)
               redisplay
               (cl-getf additional 'button-order)))
          (error (apprentice-cancel-timer)
                 (message "%S" error)))))))

(defun apprentice-insert-elisp-button (prop &optional button-order)
  (cl-destructuring-bind (tag begin end label when-clicked-form
                              &key face redisplay name
                              arguments skippable &allow-other-keys)
      prop
    (setf face (or face 'font-lock-type-face))
    (let ((keymap (make-sparse-keymap))
          (buf (current-buffer))) ; apprentice buffer
      (if name
          (define-key keymap (kbd "RET")
            'apprentice-activate-elisp-button)
        (define-key keymap (kbd "RET")
          'apprentice-activate-elisp-button))
      ;; Begin and end are zero-based, so we add one.
      (put-text-property (1+ begin) (1+ end) 'keymap keymap)
      (unless (eq :unspecified face)
        (put-text-property (1+ begin) (1+ end)
                           (if font-lock-mode
                               'font-lock-face
                             'face)
                           face))
      (put-text-property (1+ begin) (1+ end)
                         'apprentice-button t)
      (when button-order
        (put-text-property (1+ begin) (1+ end)
                           'apprentice-button-order
                           button-order))
      (put-text-property (1+ begin) (1+ end)
                         'apprentice-button-type 'elisp)
      (put-text-property (1+ begin) (1+ end)
                         'apprentice-button-properties
                         prop)
      (put-text-property (1+ begin) (1+ end)
                         'apprentice-button-additional-properties
                         (list 'button-order button-order
                               'skippable skippable
                               'current-buffer buf)))))

(defun apprentice-activate-button-then-next ()
  (interactive)
  (let ((type (get-text-property (point) 'apprentice-button-type))
        (buf (current-buffer)))
    (cl-case type
      (elisp (apprentice-activate-elisp-button))
      (lisp (apprentice-activate-lisp-button)))
    ;; Hack: This function is only used for suggestions, and the
    ;; results look prettier if we add a newline between each
    ;; suggestion. Ideally this should be handled elsewhere.
    (unless (eq (current-buffer) buf)
      (insert "\n"))
    (select-window (get-buffer-window buf))
    (apprentice-next-button)))

(defun apprentice-activate-lisp-button ()
  (interactive)
  (let ((prop (get-text-property
               (point)
               'apprentice-button-properties))
        (additional (get-text-property
                     (point)
                     'apprentice-button-additional-properties)))
    (when prop
      (cl-destructuring-bind (tag begin end label when-clicked-form
                                  &key face redisplay name
                                  arguments &allow-other-keys)
          prop
        ;; (message "%S" when-clicked-form)
        (condition-case-unless-debug error
            (progn
              (if (stringp when-clicked-form)
                  (slime-eval (progn `(cl:eval
                                       (cl:read-from-string
                                        ,when-clicked-form))
                                     t)) ; may otherwise return unreadable objects
                (slime-eval when-clicked-form)))
          (error (apprentice-cancel-timer)
                 (setf redisplay nil)
                 (message "%S" error)))
        (when redisplay
          (apprentice-update-apprentice-buffer
           (cl-getf additional 'buf))
          (when (cl-getf additional 'button-order)
            (apprentice-jump-to-nth-button
             (cl-getf additional 'button-order))))))))

(defun apprentice-insert-lisp-button (prop &optional button-order)
  (cl-destructuring-bind (tag begin end label when-clicked-form
                              &key face redisplay name
                              arguments skippable &allow-other-keys)
      prop
    (setf face (or face 'font-lock-keyword-face))
    (let ((keymap (make-sparse-keymap))
          (buf (current-buffer)))       ; apprentice buffer
      (define-key keymap (kbd "RET")
        'apprentice-activate-lisp-button)
      ;; Begin and end are zero-based, so we add one.
      (put-text-property (1+ begin) (1+ end) 'keymap keymap)
      (unless (eq :unspecified face)
        (put-text-property (1+ begin) (1+ end)
                           (if font-lock-mode
                               'font-lock-face
                             'face)
                           face))
      (put-text-property (1+ begin) (1+ end)
                         'apprentice-button t)
      (when button-order
        (put-text-property (1+ begin) (1+ end)
                           'apprentice-button-order
                           button-order))
      (put-text-property (1+ begin) (1+ end)
                         'apprentice-button-type 'lisp)
      (put-text-property (1+ begin) (1+ end)
                         'apprentice-button-properties
                         prop)
      (put-text-property (1+ begin) (1+ end)
                         'apprentice-button-additional-properties
                         (list 'button-order button-order
                               'skippable skippable
                               'current-buffer buf)))))

(defun apprentice-insert-help-line ()
  (if apprentice-locked-p
      (insert apprentice-locked-help-line)
    (insert apprentice-help-line)))

(defun apprentice-create-ephemeral-function (prop)
  (cl-destructuring-bind (tag &key name lambda-string) prop
    (push (cons name (let ((byte-compile-warnings nil))
                       (byte-compile (car (read-from-string lambda-string)))))
          apprentice-ephemeral-functions)))

(defun apprentice-call-ephemeral (name &rest args)
  (let ((func (cdr (assoc name apprentice-ephemeral-functions))))
    (if func
        (apply func args)
      (error "No ephemeral function found: %S" name))))

;; "Append"?
(defun apprentice-insert (string &optional properties)
  (let ((offset (point-max)))
    (setf apprentice-ephemeral-functions nil)
    (insert string)
    (when properties
      ;; Hack: Assumes properties are reversed, i.e. last button
      ;; first.  Button-count is used to jump to the vincinity of the
      ;; button that was clicked (with :redisplay t), instead of
      ;; ending up at point-min.
      (let ((button-count
             (1+ (cl-count-if
                  (lambda (x)
                    (member (car x) '(apprentice::lisp-button
                                      apprentice::elisp-button)))
                  properties))))
        (dolist (prop properties)
          (cl-case (car prop)
            (apprentice::lisp-button
             (apprentice-insert-lisp-button
              prop (cl-decf button-count)))
            (apprentice::elisp-button
             (apprentice-insert-elisp-button
              prop (cl-decf button-count)))
            (apprentice::fontify-region
             (apprentice-fontify-lisp-region prop))
            (apprentice::indent-region
             (apprentice-indent-region prop))
            (apprentice::add-face
             (apprentice-add-face prop))
            (apprentice::ephemeral-function
             (apprentice-create-ephemeral-function prop))
            (t (warn "Bad property %S" prop))))))
    (goto-char (point-min))
    ;; Insert the help line last, so that the property offsets work
    ;; directly. Consider: offsets could be relative.
    (apprentice-insert-help-line)))

(defun apprentice-beginning-of-button-p ()
  (cond ((= (point) (point-max))
         nil)
        ((= (point) (point-min))
         (text-property-any (point)
                            (1+ (point))
                            'apprentice-button t))
        ((and (text-property-any (1- (point))
                                 (point)
                                 'apprentice-button nil)
              (text-property-any (point)
                                 (1+ (point))
                                 'apprentice-button t))
         t)))

(defun apprentice-end-of-button-p ()
  (cond ((= (point) (point-min))
         nil)
        ((= (point) (point-max))
         (text-property-any (1- (point))
                            (point)
                            'apprentice-button t))
        ((and (text-property-any (1- (point))
                                 (point)
                                 'apprentice-button t)
              (text-property-any (point)
                                 (1+ (point))
                                 'apprentice-button nil))
         t)))

(defun apprentice-button-order (position)
  (cl-getf (get-text-property
            position
            'apprentice-button-additional-properties)
           'button-order))

(defun apprentice-next-button (&optional no-skip)
  (interactive)
  (when (= (point-max) (point))
    (goto-char (point-min)))
  (cl-tagbody
   again
   (goto-char (next-single-char-property-change
               (point)
               'apprentice-button-order))
   ;; On a button?
   (unless (apprentice-button-order (point))
     (goto-char (next-single-char-property-change
                 (point)
                 'apprentice-button-order)))
   (when (and (null no-skip)
              (< (point) (point-max))
              (cl-getf (get-text-property
                        (point)
                        'apprentice-button-additional-properties)
                       'skippable))
     (go again))))

(defun apprentice-previous-button (&optional no-skip)
  (interactive)
  (when (= (point-min) (point))
    (goto-char (point-max)))
  (cl-tagbody
   again
   (goto-char (previous-single-char-property-change
               (point)
               'apprentice-button-order))
   ;; On a button?
   (unless (apprentice-button-order (point))
     (goto-char (previous-single-char-property-change
                 (point)
                 'apprentice-button-order)))
   (when (and (null no-skip)
              (< (point-min) (point))
              (cl-getf (get-text-property
                        (point)
                        'apprentice-button-additional-properties)
                       'skippable))
     (go again))))

(defun apprentice-jump-to-nth-button (count)
  (goto-char (point-min))
  (when (< 0 count)
    (cl-block end
      (dotimes (n count)
        (apprentice-next-button t)
        (when (= (point-max) (point))
          (cl-return-from end))))))

(defun apprentice-check-apprentice-buffer ()
  (unless (eql major-mode 'apprentice-mode)
    (error "Not an apprentice buffer.")))

(defun apprentice-buffer-p (buf)
  (let ((mode (buffer-local-value 'major-mode buf)))
    (eql mode 'apprentice-mode)))

(defun apprentice-window-p (win)
  (apprentice-buffer-p (window-buffer win)))

(defun apprentice-faster-polling ()
  (interactive)
  (setf apprentice-polling-frequency
        (/ apprentice-polling-frequency 2.0))
  (apprentice-reinitialize-timer)
  (message "%s" apprentice-polling-frequency))

(defun apprentice-slower-polling ()
  (interactive)
  (setf apprentice-polling-frequency
        (* apprentice-polling-frequency 2.0))
  (apprentice-reinitialize-timer)
  (message "%s" apprentice-polling-frequency))

;;; Input should be:

;; thing at point (symbol name, number, presentation)
;; character at point

;; Context can contain:

;; package
;; filename
;; point
;; current-line
;; max-line

(defun apprentice-clear-buffer-input (buffer)
  (with-current-buffer buffer
    (setf apprentice-presentation-id nil)
    (setf apprentice-variable-name nil)
    (setf apprentice-package nil)
    (setf apprentice-form-string nil)
    (setf apprentice-looking-at nil)))

(defun apprentice-check-input (input)
  (unless (and (listp input)
               (symbolp (car input)))
    (error "Invalid input")))

(defun apprentice-set-buffer-input (input &optional buffer context)
  (unless buffer
    (setf buffer (current-buffer)))
  (with-current-buffer buffer
    (apprentice-check-apprentice-buffer)
    (setf apprentice-buffer-context context)
    (apprentice-check-input input)
    (setf apprentice-input input)))

(defun apprentice-retrieve-description-for-buffer ()
  (let* ((context (cdr apprentice-input))
         (eval-form
          (cl-case (car apprentice-input)
            (presentation
             `(apprentice:presentation-description
               ,(apprentice-input-property :id)))
            (symbol
             `(apprentice:symbol-description
               ,(apprentice-input-property :name)))
            (looking-at
             `(apprentice:character-description
               ,(apprentice-input-property :preceding-char)
               ,(apprentice-input-property :following-char)))
            (form
             ;; Ensure the package is fixed in this buffer. This
             ;; should not be needed.
             (unless (apprentice-input-property :package)
               (apprentice-set-input-property
                :package
                (slime-eval `(cl:package-name cl:*package*))))
             `(cl:progn
               (apprentice:form-description
                ,(apprentice-input-property :string)
                ,(apprentice-input-property :package)))))))
    (unless eval-form
      (error "Missing variable or presentation."))
    (condition-case nil
        (slime-eval
         `(cl:let ((apprentice::*force-return-description*
                    ,apprentice-force-update)
                   (apprentice::*buffer-context*
                    (cl:quote ,context)))
                  ,eval-form))
      (error (apprentice-cancel-timer)
             (message "Error retrieving description. Are we consing yet?")
             nil))))

(defun apprentice-create-apprentice-buffer ()
  (let ((buffer (get-buffer-create apprentice-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'apprentice-mode)
        (apprentice-mode))
      buffer)))

(defun apprentice-update-apprentice-buffer (&optional buffer input)
  (interactive)
  (unless buffer
    (setf buffer (current-buffer)))
  (with-current-buffer buffer
    (apprentice-check-apprentice-buffer)
    (when input
      (apprentice-set-buffer-input input buffer))
    (when apprentice-input
      (let ((results (apprentice-retrieve-description-for-buffer)))
        ;; (message "%S" results)
        (cond ((null results)
               nil)
              ((eql results :unchanged)
               nil)
              ((eql results :max-size-exceeded)
               (erase-buffer)
               (apprentice-insert "[Max size exceeded]"))
              ((stringp results)
               (erase-buffer)
               (apprentice-insert results))
              ((listp results)          ; with properties
               (erase-buffer)
               (apprentice-insert (car results) (cadr results))))))
    (goto-char (point-min))
    (run-hooks 'apprentice-update-apprentice-buffer-hook)))

(defun apprentice-update-the-apprentice-buffer (&optional input)
  (interactive)
  (let ((buffer (or (get-buffer apprentice-buffer-name)
                    (apprentice-create-apprentice-buffer))))
    (apprentice-update-apprentice-buffer buffer input)))

;;; Examining source code

(defun apprentice-at-toplevel-p ()
  (save-excursion
    (let ((top t))
      (ignore-errors (up-list -1)
                     (setf top nil))
      top)))

(defun apprentice-goto-toplevel ()
  (ignore-errors
    (while (not (apprentice-at-toplevel-p))
      (up-list -1)))
  (unless (zerop (current-column))
    (backward-sexp)))

(defun apprentice-toplevel-form-name ()
  (save-excursion
    (ignore-errors
      (progn
        (apprentice-goto-toplevel)
        (down-list)
        (forward-sexp 2)
        (thing-at-point 'symbol t)))))

(defun apprentice-car-of-list ()
  (when (looking-at "( *\\_<") ; list with symbol at car
    (save-excursion
      (search-forward-regexp "\\_<")
      (thing-at-point 'symbol t))))

(defun apprentice-surrounding-sexp-car ()
  (save-excursion
    (ignore-errors
      (up-list -1)
      (when (looking-at "( *\\_<") ; list with symbol at car
        (search-forward-regexp "\\_<")
        (thing-at-point 'symbol t)))))

(defun apprentice-current-form-path ()
  (let* (path)
    (save-excursion
      (ignore-errors
        (while (not (zerop (current-column)))
          (up-list -1)
          (push (apprentice-car-of-list) path))))
    path))

(defun apprentice-enclosing-form-position ()
  (condition-case nil
      (save-excursion
        (up-list -1 t)
        (when (eql ?\" (char-after (point)))
          (up-list -1 t))
        (point))
    (error nil)))

(defun apprentice-enclosing-form ()
  (condition-case nil
      (let ((enclosing-form-position
             (apprentice-enclosing-form-position)))
        (save-excursion
          (up-list -1 t)
          (when (eql ?\" (char-after (point)))
            (up-list -1 t))
          (forward-sexp)
          (buffer-substring-no-properties
           enclosing-form-position
           (point))))
    (error nil)))

(defun apprentice-toplevel-form ()
  (condition-case nil
      (save-excursion
        (let (begin)
          (beginning-of-defun)
          (setf begin (point))
          (forward-list)
          (buffer-substring-no-properties
           begin (point))))
    (error nil)))

(defun apprentice-package ()
  (condition-case nil
      (slime-current-package)
    (error nil)))

(defun apprentice-build-context ()
  (when apprentice-provide-context
    `(,@(when (member 'point apprentice-provide-context)
          (list :point (point)))
      ,@(when (member 'column apprentice-provide-context)
          (list :column (current-column)))
      ,@(when (member 'region apprentice-provide-context)
          (list :region (if (region-active-p)
                            (list (region-beginning)
                                  (region-end))
                          nil)))
      ,@(when (member 'line apprentice-provide-context)
          (list :line (line-number-at-pos)))
      ,@(when (member 'max-line apprentice-provide-context)
          (list :max-line (line-number-at-pos (point-max))))
      ,@(when (member 'enclosing-form apprentice-provide-context)
          (list :enclosing-form (apprentice-enclosing-form)))
      ,@(when (member 'toplevel-form apprentice-provide-context)
          (list :toplevel-form (apprentice-toplevel-form)))
      ,@(when (member 'package apprentice-provide-context)
          (list :package (apprentice-package)))
      ,@(when (member 'filename apprentice-provide-context)
          (list :filename (buffer-file-name)))
      ,@(when (member 'locked apprentice-provide-context)
          (list :locked apprentice-locked-p)))))

(defun apprentice-determine-input-at-point ()
  (cl-block determine-input
    ;; Presentation?
    (let* ((presentation
            (car (slime-presentation-around-or-before-point
                  (point))))
           (presentation-id (when presentation
                              (slime-presentation-id presentation))))
      (when presentation-id
        (cl-return-from determine-input
          `(presentation :id ,presentation-id
                         ,@(apprentice-build-context)))))
    ;; Symbol?
    (let* ((symbol (thing-at-point 'symbol t)))
      (when symbol
        (cl-return-from determine-input
          `(symbol :name ,symbol ,@(apprentice-build-context)))))
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
                     ,@(apprentice-build-context))))))

(defun apprentice-set-input-from-point-maybe ()
  (let ((input (apprentice-determine-input-at-point)))
    (apprentice-set-buffer-input
     input
     apprentice-buffer-name)))

(defun apprentice-update-if-live-window (window)
  (let ((buf (window-buffer window)))
    (when (and (apprentice-buffer-p buf)
               (not (eql (current-buffer) buf))
               ;; Don't try to update if the timer has been cancelled.
               apprentice-describe-timer)
      (apprentice-update-apprentice-buffer buf))))

(defun apprentice-timer-function ()
  (interactive)
  (unless apprentice-inhibit-update-p
    (cond ((and apprentice-eager-p
                (eql major-mode 'lisp-mode)
                (get-buffer apprentice-buffer-name)
                (not (get-buffer-window "*Fuzzy Completions*"))
                (not (get-buffer-window "*Completions*"))
                (not (get-window-with-predicate
                      #'apprentice-window-p
                      nil t)))
           (apprentice-display-apprentice-buffer))
          ((and (or (eql major-mode 'lisp-mode)
                    (eql major-mode 'slime-repl-mode))
                (get-buffer apprentice-buffer-name))
           (unless (with-current-buffer (get-buffer
                                         apprentice-buffer-name)
                     (and apprentice-input
                          (eq 'form (car apprentice-input))))
             (apprentice-set-input-from-point-maybe))
           (walk-windows #'apprentice-update-if-live-window
                         nil t)))))

(defun apprentice-reinitialize-continuous-timer ()
  (interactive)
  (ignore-errors (cancel-timer apprentice-describe-timer))
  (setf apprentice-update-mode 'continuous)
  (setf apprentice-describe-timer
        (run-with-timer 0
                        apprentice-polling-frequency
                        'apprentice-timer-function))
  (message "Continuous mode."))

(defun apprentice-reinitialize-idle-timer ()
  (interactive)
  (ignore-errors (cancel-timer apprentice-describe-timer))
  (setf apprentice-update-mode 'idle)
  (setf apprentice-describe-timer
        (run-with-idle-timer apprentice-polling-frequency
                             t
                             'apprentice-timer-function))
  (message "Idle mode."))

(defun apprentice-reinitialize-timer ()
  (interactive)
  (ignore-errors (cancel-timer apprentice-describe-timer))
  (if (eql apprentice-update-mode 'idle)
      (apprentice-reinitialize-idle-timer)
    (apprentice-reinitialize-continuous-timer)))

(defun apprentice-toggle-update-mode ()
  (interactive)
  (if (eql apprentice-update-mode 'idle)
      (apprentice-reinitialize-continuous-timer)
    (apprentice-reinitialize-idle-timer)))

(defun apprentice-cancel-timer ()
  (interactive)
  (cancel-timer apprentice-describe-timer)
  (setf apprentice-describe-timer nil)
  (save-excursion
    (walk-windows
     (lambda (win)
       (with-current-buffer (window-buffer win)
         (when (apprentice-window-p win)
           (message "Trying %s" win)
           (goto-char (point-min))
           (end-of-line)
           (insert (propertize " INACTIVE"
                               'face '(:foreground "red")
                               'font-lock-face '(:foreground "red"))))))
     nil t)))

(defun apprentice-input-property (property-name)
  (cl-getf (cdr apprentice-input) property-name))

(defun apprentice-set-input-property (property-name value)
  (setf (cl-getf (cdr apprentice-input) property-name) value))

(defun apprentice-lock-apprentice ()
  (interactive)
  (apprentice-check-apprentice-buffer)
  (let ((buffer-name
         (format "%s:%s"
                 apprentice-buffer-name
                 (cl-case (car apprentice-input)
                   (symbol (apprentice-input-property :name))
                   (presentation (apprentice-input-property :id))
                   (looking-at (apprentice-input-property
                                :following-char))
                   (form (let ((string (apprentice-input-property
                                        :string)))
                           (when string
                             (substring string 0 (min 20 (length string))))))))))
    (cond ((get-buffer buffer-name)
           (switch-to-buffer buffer-name))
          (t (rename-buffer buffer-name)
             (setf apprentice-locked-p t)
             (let ((apprentice-force-update t))
               (apprentice-update-apprentice-buffer buffer-name))
             (apprentice-create-apprentice-buffer)))))

(defun apprentice-lock-apprentice-and-split ()
  (interactive)
  (apprentice-lock-apprentice)
  (split-window)
  (switch-to-buffer apprentice-buffer-name))

(defun apprentice-display-apprentice-buffer ()
  (apprentice-create-apprentice-buffer)
  (apprentice-set-input-from-point-maybe)
  (let ((apprentice-force-update t))
    (apprentice-update-the-apprentice-buffer))
  (display-buffer apprentice-buffer-name))

;; Interactive function
(defun apprentice-describe (prefix)
  (interactive "P")
  (if prefix
      (apprentice-describe-form)
    (progn
      (unless apprentice-describe-timer
        (apprentice-reinitialize-timer))
      (apprentice-display-apprentice-buffer))))

;; Interactive function
(defun apprentice-describe-form ()
  (interactive)
  (let ((form (save-excursion
                (let (begin end)
                  (backward-sexp)
                  (setf begin (point))
                  (forward-sexp)
                  (setf end (point))
                  (buffer-substring-no-properties begin end)))))
    (unless apprentice-describe-timer
      (apprentice-reinitialize-timer))
    (apprentice-create-apprentice-buffer)
    (apprentice-set-buffer-input
     (list 'form
           :string form
           :package (apprentice-package))
     apprentice-buffer-name
     (apprentice-build-context))
    (let ((apprentice-force-update t))
      (apprentice-update-the-apprentice-buffer))
    (display-buffer apprentice-buffer-name)))

