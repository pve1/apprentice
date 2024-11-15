;;;; Requires
;;;;   apprentice
;;;;   "caching-apprentice"

(in-package :apprentice) cx

(defclass Apprentice-gathering ()
  ((apprentices :initarg :apprentices
                :accessor Apprentices
                :initform nil)))

(defmethod Apprentice-gathering-divider (apprentice)
  "-------------------------------------------------------------")

(defmethod Apprentices-based-on-input (apprentice input)
  (apprentices apprentice))

(defmethod describe-with-apprentice ((ap apprentice-gathering)
                                     object
                                     stream)
  (let (divider-printed)
    (flet ((print-divider-maybe ()
             (unless divider-printed
               (fresh-line stream)
               (with-face ("apprentice-divider" stream)
                 (princ (apprentice-gathering-divider ap) stream))
               (fresh-line stream)
               (setf divider-printed t))))
      (dolist (a (apprentices-based-on-input ap object))
        (print-divider-maybe)
        (when (describe-with-apprentice a object stream)
          (fresh-line stream)
          (setf divider-printed nil)))
      ;; Print one here as well, to make it look consistent,
      ;; regardless of what the last apprentice printed.
      (print-divider-maybe))))

(defun instantiate-maybe (thing)
  (typecase thing
    (null thing)
    (keyword thing)
    (symbol (make-instance thing))
    (t thing)))

(defmethod initialize-instance :after ((a apprentice-gathering)
                                       &key apprentices)
  (setf (apprentices a) (mapcar 'instantiate-maybe apprentices)))

(defun apprentice-gathering (&rest apprentices)
  (make-instance 'apprentice-gathering
    :apprentices (mapcar 'instantiate-maybe apprentices)))

(defun make-example-apprentice-gathering ()
  (let ((*caching-apprentice-default-update-interval* 2))
    (apprentice-gathering 'describe-apprentice
                          'value-apprentice
                          'grep-apprentice
                          'apropos-apprentice)))
