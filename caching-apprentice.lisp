;;;; Requires
;;;;   apprentice

(in-package :apprentice) cx

(defvar *Caching-apprentice-default-update-interval* 5)

(defclass Caching-apprentice ()
  ((last-updated :initarg :last-updated
                 :accessor Last-updated
                 :initform 0)
   (last-input :initarg :last-input
               :accessor Last-input
               :initform nil)
   (update-interval
    :initarg :update-interval
    :accessor Update-interval
    :initform *caching-apprentice-default-update-interval*)
   (last-result :initarg :last-result
                :accessor Last-result
                :initform nil)
   (busy-result :initarg :busy-result
                :accessor Busy-result
                :initform " ... ")))

(defmethod Apprentice-same-input-as-last-time-p (apprentice
                                                 input)
  (equal (last-input apprentice) input))

(defmethod apprentice-same-input-as-last-time-p (apprentice
                                                 (input symbol))
  (and (symbolp (last-input apprentice))
       (equal (symbol-name (last-input apprentice))
              (symbol-name input))))

(defmethod Apprentice-need-update-p (apprentice input)
  (if (<= (update-interval apprentice)
          (- (get-universal-time)
             (last-updated apprentice)))
      t
      (if (apprentice-same-input-as-last-time-p apprentice input)
          nil
          t)))

(defmethod Apprentice-can-update-p (apprentice input)
  (<= (update-interval apprentice)
      (- (get-universal-time)
         (last-updated apprentice))))

(defmethod Apprentice-mark-updated (apprentice input)
  (setf (last-updated apprentice) (get-universal-time)
        (last-input apprentice) input))

(defmethod Apprentice-cache-results (apprentice input result)
  (setf (last-result apprentice) result))

(defgeneric Apprentice-update (apprentice input)
  (:documentation ""))

(defmethod apprentice-update (apprentice input)
  "")

(defmethod Apprentice-update-maybe (apprentice input)
  (let ((can (apprentice-can-update-p apprentice input))
        (need (apprentice-need-update-p apprentice input)))
    (cond ((and can need)
           (prog1 (apprentice-cache-results
                   apprentice input
                   (apprentice-update apprentice input))
             (apprentice-mark-updated apprentice input)))
          ((and need (not can))
           (busy-result apprentice))
          ((not need)
           (last-result apprentice)))))

(defmethod describe-with-apprentice ((ap caching-apprentice)
                                     object
                                     stream)
  (let* ((*standard-output* stream)
         (result (apprentice-update-maybe ap object)))
    (when result
      (princ result stream)
      t)))
