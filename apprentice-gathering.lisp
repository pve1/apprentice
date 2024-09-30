;;;; Requires
;;;;   slime-apprentice
;;;;   "caching-apprentice"

(in-package :slime-apprentice) cx

(defclass Apprentice-gathering ()
  ((apprentices :initarg :apprentices
                :accessor apprentices
                :initform nil)))

(defmethod Apprentice-gathering-divider ((ap apprentice-gathering))
  "------------------------------------------------------------")

(defmethod describe-with-apprentice ((ap apprentice-gathering)
                                     object
                                     stream)
  (let (divider-printed)
    (flet ((print-divider-maybe ()
             (unless divider-printed
               (fresh-line stream)
               (princ (apprentice-gathering-divider ap) stream)
               (fresh-line stream)
               (setf divider-printed t))))
      (dolist (a (apprentices ap))
        (print-divider-maybe)
        (when (describe-with-apprentice a object stream)
          (fresh-line stream)
          (setf divider-printed nil)))
      ;; Print one here as well, to make it look consistent,
      ;; regardless of what the last apprentice printed.
      (print-divider-maybe))))

(defmethod initialize-instance :after ((a apprentice-gathering) &key apprentices)
  (setf (apprentices a) (mapcar (lambda (x)
                                  (if (symbolp x)
                                      (make-instance x)
                                      x))
                                apprentices)))

(defun apprentice-gathering (&rest apprentices)
  (make-instance 'apprentice-gathering
    :apprentices (mapcar (lambda (x)
                           (if (symbolp x)
                               (make-instance x)
                               x))
                         apprentices)))

(defun Make-example-apprentice-gathering ()
  (let ((*caching-apprentice-default-update-interval* 2))
    (apprentice-gathering 'describe-apprentice
                          'value-apprentice
                          'grep-apprentice
                          'apropos-apprentice)))


