;;;; Requires
;;;;   "slime-apprentice"
;;;;   "buttons"

(in-package :slime-apprentice) cx

(defclass Toplevel-apprentice (caching-apprentice)
  ((busy-result :initarg :busy-result
                :accessor busy-result
                :initform "Toplevel forms: ...")))

(defmethod apprentice-update ((ap toplevel-apprentice) object)
  (let ((current-file (getf *buffer-context* :filename))
        (offset (file-position *standard-output*))
        (string))
    (when current-file
      (setf string
            (with-output-to-string (result)
              (format result "Toplevel forms:~%~%")
              (alexandria:with-input-from-file (s current-file)
                (loop :for line = (read-line s nil s)
                      :for line-number :from 1
                      :until (eq line s)
                      :when (and (< 0 (length line))
                                 (not (member (alexandria:first-elt line)
                                              '(#\Space #\Tab #\;))))
                      :do (put-elisp-button-here 
                          ap 
                          line
                          `(progn 
                             (goto-line ,line-number
                                        (get-file-buffer 
                                         ,(namestring current-file))))
                          :offset offset
                          :stream result)
                         (terpri result)))))
      
      (alexandria:appendf *description-properties*
                          *description-properties* 
                          (list `(fontify-region ,offset
                                                 ,(+ offset (length string)))))
      string)))
