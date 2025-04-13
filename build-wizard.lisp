;;;; Requires
;;;;   apprentice
;;;;   "buttons"
;;;;   "emacs"
;;;;   "option"

;;; Lets the user choose between several options.

;;; Note: Empty options will not be displayed by the wizard. An empty
;;; option is created with (option "ABC" nil).

(in-package :apprentice) cx

(defclass build-wizard ()
  ;; Plist
  ((options :initarg :options
            :accessor options
            :initform nil)
   (initargs :initarg :initargs
             :accessor initargs
             :initform nil))
  (:documentation ""))

(defmethod initialize-instance :after ((w build-wizard)
                                       &rest initargs
                                       &key &allow-other-keys)

  (setf (initargs w) initargs)
  (let (empty)
    (alexandria:doplist (key val (options w))
      (when (null val)
        (warn "Null option for key ~S." key)
        (push key empty)))
    (dolist (e empty)
      (setf (getf (options w) e)
            (option "Empty" nil)))))

(defmethod wizard-option (wizard option-keyword)
  (getf (options wizard) option-keyword))

(defmethod wizard-display ((wiz build-wizard) &key)
  (loop :for (key opt) :on (options wiz) :by #'cddr
        :do (wizard-display-option wiz opt)))

(defmethod wizard-make-plist ((wiz build-wizard))
  (loop :for (key opt) :on (options wiz) :by #'cddr
        :collect key
        :collect (option-selected-suboption-value opt)))

(defmethod wizard-display-option ((wiz build-wizard) option
                                  &key (on-select (constantly nil))
                                       (display-suboptions t)
                                       alternatives-to-display)

  (let* ((alternatives (or alternatives-to-display
                           (alternatives option)))
         (labels-to-display (mapcar 'option-alternative-label
                                    alternatives)))
    (when labels-to-display
      (format t "~A: " (name option))
      (loop :for label :in labels-to-display
            :do (let ((label* label))
                  (put-lisp-button-here
                   *apprentice*
                   (format nil "[~A] " label*)
                   (lambda (appr)
                     (set-temporary-apprentice appr)
                     (option-select option label*)
                     (funcall on-select
                              :apprentice appr
                              :option option
                              :label label*)
                     (emacs-message
                      (option-selected-message option)))
                   :redisplay t)))
      (terpri)
      ;; Recursively show suboptions?
      (alexandria:when-let ((show? display-suboptions)
                            (suboptions (option-get-suboptions
                                         option)))
        (wizard-display-option wiz suboptions)))))
