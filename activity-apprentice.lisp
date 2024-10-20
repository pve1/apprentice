;;;; Requires
;;;;   "apprentice"
;;;;   "buttons"

(in-package :apprentice) cx

(defclass Activity-apprentice ()
  ((activity-history :initarg :activity-history
                     :accessor activity-history
                     :initform nil)
   (history-length :initarg :history-length
                    :accessor history-length
                    :initform 10)
   (proximity-cutoff :initarg :proximity-cutoff
                     :accessor proximity-cutoff
                     :initform 20)))

(defmethod activity-apprentice-tick (apprentice)
  (with-accessors ((proximity-cutoff proximity-cutoff)
                   (history-length history-length)
                   (activity-history activity-history))
      apprentice
    (let* ((line (getf *buffer-context* :line))
           (file (getf *buffer-context* :filename))
           (prox proximity-cutoff)
           (find-predicate
             (lambda (x)
               (and (equal (getf x :file) file)
                    (< (abs (- line (getf x :line)))
                       prox)))))
      (when file
        ;; Remove nearby activity.
        (setf activity-history (remove-if find-predicate
                                          activity-history))
        (push (list :line line :file file) activity-history)
        (when (< history-length (length activity-history))
          (setf activity-history (subseq activity-history
                                         0 history-length)))))))

(defmethod activity-history-entry-button (ap entry)
  (destructuring-bind (&key line file) entry
    (put-elisp-button-here
     ap (format nil "~A: ~A"
                (file-namestring file)
                line)
     nil
     :name 'activity-apprentice-button
     :arguments (list line file))))

(defmethod apprentice-create-ephemerals ((ap activity-apprentice))
  (create-ephemeral-elisp-function
   ap 'activity-apprentice-button
   '(lambda (line-number file)
     (let ((current-window (get-buffer-window)))
       (goto-line line-number
                  (find-file-other-window
                   file))
       (recenter-top-bottom)
       (if current-window
           (select-window current-window)
           (other-window 1))))))

(defmethod describe-with-apprentice ((ap activity-apprentice)
                                     object
                                     stream)
  (let* ((*standard-output* stream))
    (activity-apprentice-tick ap)
    (when (activity-history ap)
      (apprentice-create-ephemerals ap)
      (format t "Recent activity:~2%")
      (loop :for entry :in (activity-history ap)
            :do (activity-history-entry-button ap entry)
                (terpri))
      t)))
