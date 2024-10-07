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

(defmethod apprentice-same-input-as-last-time-p (object input)
  (equal (last-input object) input))

(defmethod apprentice-same-input-as-last-time-p (object (input symbol))
  (and (symbolp (last-input object))
       (equal (symbol-name (last-input object))
              (symbol-name input))))

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
           (last-result object)))))

(defmethod describe-with-apprentice ((ap caching-apprentice)
                                     object
                                     stream)
  (let* ((*standard-output* stream)
         (result (apprentice-update-maybe ap object)))
    (when result
      (princ result stream)
      t)))
