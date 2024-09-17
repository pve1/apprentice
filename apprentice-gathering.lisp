(in-package :slime-apprentice)

(defclass apprentice-gathering ()
  ((apprentices :initarg :apprentices
                :accessor apprentices
                :initform nil)))

(defmethod apprentice-gathering-divider ((appr apprentice-gathering))
  "------------------------------------------------------------")

(defmethod describe-with-apprentice ((appr apprentice-gathering)
                                     (object symbol)
                                     stream)
  (dolist (a (apprentices appr))
    (when (describe-with-apprentice a object stream)
      (fresh-line stream)
      (princ (apprentice-gathering-divider appr) stream)
      (terpri stream))))

(defun apprentice-gathering (&rest apprentices)
  (make-instance 'apprentice-gathering
    :apprentices (mapcar (lambda (x)
                           (if (symbolp x)
                               (make-instance x)
                               x))
                         apprentices)))

(defun make-example-apprentice-gathering ()
  (let ((*caching-apprentice-default-update-interval* 2))
    (apprentice-gathering 'describe-apprentice
                          'value-apprentice
                          'grep-apprentice
                          'apropos-apprentice)))


