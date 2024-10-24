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
                   :initform 5)
   (proximity-cutoff :initarg :proximity-cutoff
                     :accessor proximity-cutoff
                     :initform 60)
   (button-colors :initarg :button-colors
                  :accessor button-colors
                  :initform '((:foreground "white")
                              (:foreground "#aaaaffffffff")
                              (:foreground "#5555ffffffff")
                              (:foreground "cyan")))))

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

(defun emacs-message (object)
  (swank:eval-in-emacs `(message ,(princ-to-string object))))

(defmethod activity-adjust-proximity ((ap activity-apprentice)
                                      method)
  (with-accessors ((proximity-cutoff proximity-cutoff)
                   (history-length history-length)
                   (activity-history activity-history))
      ap
    (case method
      (:double (when (<= 10 proximity-cutoff)
                 (setf proximity-cutoff (* 2 proximity-cutoff))))
      (:halve (if (<= 20 proximity-cutoff)
                  (setf proximity-cutoff
                        (floor proximity-cutoff 2))
                  (setf proximity-cutoff 10)))
      (:inc (when (<= 10 proximity-cutoff)
                   (incf proximity-cutoff 10)))
      (:dec (if (<= 20 proximity-cutoff)
                (decf proximity-cutoff 10)
                (setf proximity-cutoff 10))))
    (emacs-message (format nil "Proximity: ~A"
                           proximity-cutoff))))

(defmethod describe-with-apprentice ((ap activity-apprentice)
                                     object
                                     stream)
  (let* ((*standard-output* stream))
    (flet ((color (n)
             (nth n (button-colors ap))))
      (activity-apprentice-tick ap)
      (when (activity-history ap)
        (apprentice-create-ephemerals ap)
        (format t "Recent activity: ")
        (put-lisp-button-here
         ap "[P" `(activity-adjust-proximity *button-apprentice* :halve)
         :face (color 0))
        (put-lisp-button-here
         ap "R" `(activity-adjust-proximity *button-apprentice* :dec)
         :face (color 1))
        (put-lisp-button-here
         ap "O" `(activity-adjust-proximity *button-apprentice* :inc)
         :face (color 2))
        (put-lisp-button-here
         ap "X]" `(activity-adjust-proximity *button-apprentice* :double)
         :face (color 3))
        (terpri)
        (terpri)
        (loop :for entry :in (activity-history ap)
              :do (activity-history-entry-button ap entry)
                  (terpri))
        t))))



