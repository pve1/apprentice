;;;; Requires
;;;;   closer-mop
;;;;   "apprentice"
;;;;   "apprentice-gathering"
;;;;   "apropos-apprentice"
;;;;   "grep-apprentice"
;;;;   "describe-apprentice"
;;;;   "value-apprentice"
;;;;   "method-apprentice"
;;;;   "toplevel-apprentice"
;;;;   "suggest-apprentice"
;;;;   "package-apprentice"
;;;;   "buttons"

(in-package :apprentice) cx

(defclass Example-apprentice (apprentice-gathering)
  ((overview-apprentices :initarg :overview-apprentices
                         :accessor overview-apprentices
                         :initform nil))
  (:documentation ""))

(defmethod initialize-instance :after ((e example-apprentice)
                                       &key overview-apprentices)
  (setf (overview-apprentices e)
        (mapcar 'instantiate-maybe overview-apprentices)))

;; If the point is not on a symbol or number, use the overview
;; apprentices instead of the normal apprentices.
(defmethod apprentices-based-on-input ((ap example-apprentice)
                                       (input looking-at-character))
  (overview-apprentices ap))

;; Ad-hoc apprentice to reload this file.
(defun example-apprentice-reload (object stream)
  (declare (ignore object))
  (princ "                         " stream)
  (put-lisp-button-here
   *apprentice*
   "[RELOAD]"
   '(asdf:load-system "apprentice/example-apprentice" :force t)
   :stream stream))

;; This is a good starting point for creating your own
;; apprentice. Just copy and modify according to taste, then assign
;; the resulting instance to *apprentice*.
(defun Make-example-apprentice ()
  (let* ((*caching-apprentice-default-update-interval* 2)
         (activity-apprentice (make-instance 'activity-apprentice
                                :history-length 3
                                :proximity-cutoff 60)))
    (make-instance 'example-apprentice
      :overview-apprentices
      (list 'suggest-apprentice
            activity-apprentice
            'wide-toplevel-apprentice
            #'example-apprentice-reload)
      :apprentices
      (list 'suggest-apprentice
            activity-apprentice
            'describe-apprentice
            'package-apprentice
            'value-apprentice
            'grep-apprentice
            (make-instance 'apropos-apprentice
              :ignore-input-function
              (lambda (sym)
                (= (length (string sym)) 1))
              :interesting-symbol-function
              (lambda (sym)
                ;; Ignore mixed case symbols.
                (if (eq (symbol-package sym) *package*)
                    (not (find-if #'lower-case-p
                                  (symbol-name sym)))
                    t)))))))

(setf *apprentice* (make-example-apprentice))
