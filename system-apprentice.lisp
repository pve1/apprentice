;;;; Requires
;;;;   apprentice
;;;;   eclector
;;;;   "buttons"
;;;;   "emacs"
;;;;   "exec-apprentice"
;;;;   "core-apprentice"
;;;;   "system"

;;;; System apprentice

;;;; This apprentice will try to figure out to which system the
;;;; current buffer belongs and then load it (using asdf:load).

(in-package :apprentice) cx

(defclass System-apprentice ()
  ((confirm-restart :initarg :confirm-restart
                    :accessor confirm-restart
                    :initform t)
   (exec-apprentice :initarg :exec-apprentice
                    :accessor exec-apprentice
                    :initform nil)
   (core-apprentice :initarg :core-apprentice
                    :accessor core-apprentice
                    :initform nil)))

(defmethod initialize-instance :after ((s system-apprentice) 
                                       &key implementations)
  (with-accessors ((exec-apprentice exec-apprentice)
                   (core-apprentice core-apprentice)) s
    (when implementations
      (when (null exec-apprentice)
        (setf exec-apprentice (make-instance 'exec-apprentice
                                :implementations implementations)))
      (when (null (core-apprentice s))
        (setf core-apprentice (make-instance 'core-apprentice
                                :implementations implementations))))))

(defmethod System-apprentice-load ((ap system-apprentice) file)
  (alexandria:when-let* ((file file)
                         (system (source-file-system-name file)))
    (emacs-message (format nil "Loading ~A." system))
    (asdf:load-system system)
    (eval-in-emacs
     `(with-current-buffer (get-file-buffer ,file)
        (slime-sync-package-and-default-directory)))))

;;; Wow, many hoops, such jumping.
(defmethod system-apprentice-restart ((ap system-apprentice) file)
  (alexandria:when-let* ((file file)
                         (system (source-file-system-name file))
                         (confirm (if (confirm-restart ap)
                                      (eval-in-emacs
                                       `(member
                                         (read-key "Restart lisp? (Y/n)")
                                         '(13 121))) ; RET or y
                                      t))
                         (load-string
                          (let ((*package* (find-package :keyword)))
                            (prin1-to-string
                             `(asdf:load-system ',system)))))
    ;; Lexical binding not available.
    (eval-in-emacs
     `(progn
        ;; One-time hook
        (setf (symbol-function 'apprentice-example-system-restart-hook)
              (lambda ()
                (remove-hook 'slime-connected-hook
                             'apprentice-example-system-restart-hook)
                (slime-eval-async
                 (car (read-from-string ,load-string))
                 (lambda (result)
                   (with-current-buffer (get-file-buffer ,file)
                     (slime-sync-package-and-default-directory))))))
        (run-with-timer
         0.1 nil
         (lambda ()
           (add-hook 'slime-connected-hook
                     'apprentice-example-system-restart-hook
                     99)
           (slime-restart-inferior-lisp)))))
    t))

(defmethod system-apprentice-load-current ((ap system-apprentice))
  (system-apprentice-load
   ap (buffer-context-property :filename)))

(defmethod system-apprentice-restart-current ((ap system-apprentice))
  (system-apprentice-restart
   ap (buffer-context-property :filename)))

(defmethod system-apprentice-display-exec ((ap system-apprentice))
  (set-temporary-apprentice
   (exec-apprentice ap)))

(defmethod system-apprentice-display-core ((ap system-apprentice))
  (set-temporary-apprentice
   (core-apprentice ap)))

(defmethod describe-with-apprentice ((ap system-apprentice)
                                     (object looking-at-character)
                                     stream)
  (let ((*standard-output* stream))
    (alexandria:when-let ((file (buffer-context-property :filename)))
      (princ "System: ")
      (put-lisp-button-here ap
                            "[LOAD]"
                            #'system-apprentice-load-current)
      (princ " ")
      (put-lisp-button-here ap
                            "[RESTART-LOAD]"
                            #'system-apprentice-restart-current)
      (princ " ")
      (put-lisp-button-here ap
                            "[CORE]"
                            #'system-apprentice-display-core
                            :redisplay t)
      t)))

(defmethod describe-with-apprentice ((ap system-apprentice)
                                     (object symbol)
                                     stream)
  (let ((*standard-output* stream))
    (alexandria:when-let ((file (buffer-context-property :filename)))
      (princ "System: ")
      (put-lisp-button-here ap
                            "[LOAD]"
                            #'system-apprentice-load-current)
      (princ " ")
      (put-lisp-button-here ap
                            "[RESTART-LOAD]"
                            #'system-apprentice-restart-current)
      (princ " ")
      (when (and (symbol-package object)
                 (fboundp object))
        (put-lisp-button-here ap
                              "[EXEC]"
                              #'system-apprentice-display-exec
                              :redisplay t))
      t)))
