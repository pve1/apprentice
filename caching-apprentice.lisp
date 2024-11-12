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
                :initform " ... ")))

(defmethod apprentice-same-input-as-last-time-p (apprentice
                                                 input)
  (equal (last-input apprentice) input))

(defmethod apprentice-same-input-as-last-time-p (apprentice
                                                 (input symbol))
  (and (symbolp (last-input apprentice))
       (equal (symbol-name (last-input apprentice))
              (symbol-name input))))

(defmethod apprentice-need-update-p (apprentice input)
  (if (<= (update-interval apprentice)
          (- (get-universal-time)
             (last-updated apprentice)))
      t
      (if (apprentice-same-input-as-last-time-p apprentice input)
          nil
          t)))

(defmethod apprentice-can-update-p (apprentice input)
  (<= (update-interval apprentice)
      (- (get-universal-time)
         (last-updated apprentice))))

(defmethod apprentice-mark-updated (apprentice input)
  (setf (last-updated apprentice) (get-universal-time)
        (last-input apprentice) input))

(defmethod apprentice-cache-results (apprentice input result)
  (setf (last-result apprentice) result))

(defgeneric Apprentice-update (apprentice input)
  (:documentation ""))

(defmethod apprentice-update (apprentice input)
  "")

(defmethod apprentice-update-maybe (apprentice input)
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
