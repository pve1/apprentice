;;;; Requires
;;;;   apprentice

(in-package :apprentice) cx

(defvar *Caching-apprentice-default-update-interval* 5)

(defclass Caching-apprentice ()
  ((last-updated :initarg :last-updated
                 :accessor last-updated
                 :initform 0)
   (last-input :initarg :last-input
               :accessor last-input
               :initform nil)
   (update-interval
    :initarg :update-interval
    :accessor update-interval
    :initform *caching-apprentice-default-update-interval*)
   (last-result :initarg :last-result
                :accessor last-result
                :initform nil)
   (busy-result :initarg :busy-result
                :accessor busy-result
                :initform " ... ")
   (last-state :initarg :last-state
               :accessor last-state
               :initform nil)))

(defmethod apprentice-same-input-as-last-time-p (object input)
  (equal (last-input object) input))

(defmethod apprentice-same-input-as-last-time-p (object (input symbol))
  (and (symbolp (last-input object))
       (equal (symbol-name (last-input object))
              (symbol-name input))))

(defmethod last-result-with-state (object)
  (prog1 (last-result object)
    (dolist (state (last-state object))
      (funcall state))))

(defmethod apprentice-need-update-p (object input)
  (if (<= (update-interval object)
          (- (get-universal-time)
             (last-updated object)))
      t
      (if (apprentice-same-input-as-last-time-p object input)
          nil
          t)))

(defmethod apprentice-can-update-p (object input)
  (<= (update-interval object)
      (- (get-universal-time)
         (last-updated object))))

(defmethod apprentice-mark-updated (object input)
  (setf (last-updated object) (get-universal-time)
        (last-input object) input))

(defmethod apprentice-cache-results (object input result)
  (setf (last-result object) result))

(defgeneric Apprentice-update (object input)
  (:documentation ""))

(defmethod apprentice-update :before (object input)
  (setf (last-state object) nil))

(defmethod apprentice-update (object input)
  "")

(defmethod apprentice-update-maybe (object input)
  (let ((can (apprentice-can-update-p object input))
        (need (apprentice-need-update-p object input)))
    (cond ((and can need)
           (prog1 (apprentice-cache-results
                   object input
                   (apprentice-update object input))
             (apprentice-mark-updated object input)))
          ((and need (not can))
           (busy-result object))
          ((not need)
           (last-result-with-state object)))))

(defmethod describe-with-apprentice ((ap caching-apprentice)
                                     object
                                     stream)
  (let* ((*standard-output* stream)
         (result (apprentice-update-maybe ap object)))
    (when result
      (princ result stream)
      t)))

;; Must record lisp buttons so that callbacks will be properly
;; created when returning cached content.
(defmethod put-lisp-button-here :after ((ap caching-apprentice)
                                        label
                                        when-clicked
                                        &rest rest
                                        &key stream offset)
  (let ((relative-begin (- (file-position stream)
                           offset)))
    (push (lambda ()
            (let* ((begin (+ relative-begin
                             (file-position *description-stream*)))
                   (end (+ begin (length label))))
              (format *debug-io* "Pushing old button ~A~%" label)
              (push-description-property
               (apply #'make-button
                      ap 'lisp
                      label
                      (make-button-callback-form ap when-clicked)
                      :begin begin
                      :end end
                      rest))))
          (last-state ap))))
