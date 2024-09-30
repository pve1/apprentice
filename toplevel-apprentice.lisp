;;;; Requires
;;;;   "slime-apprentice"
;;;;   "buttons"

(in-package :slime-apprentice) cx

(defclass Toplevel-apprentice (caching-apprentice)
  ((busy-result :initarg :busy-result
                :accessor busy-result
                :initform "Toplevel forms: ...")))

(defmethod apprentice-same-input-as-last-time-p ((ap toplevel-apprentice)
                                                 (input looking-at-character))
  t)

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
                           `(let ((current-window
                                    (get-buffer-window)))
                              (goto-line ,line-number
                                         (get-file-buffer
                                          ,(namestring current-file)))
                              (if current-window
                                  (select-window current-window)
                                  (other-window 1)))
                           :face :unspecified
                           :offset offset
                           :stream result)
                          (terpri result)))))
      (push `(fontify-region ,offset ,(+ offset (length string)))
            *description-properties*)
      string)))

(defclass Wide-toplevel-apprentice (toplevel-apprentice)
  ())

(defmethod apprentice-update ((ap wide-toplevel-apprentice)
                              object)
  (let ((offset (file-position *standard-output*))
        (string))
    (flet ((walk (lisp-file stream)
             (alexandria:with-input-from-file (s lisp-file)
               (loop :for line = (read-line s nil s)
                     :for line-number :from 1
                     :until (eq line s)
                     :when (and (< 0 (length line))
                                (not (member (alexandria:first-elt line)
                                             '(#\Space #\Tab #\;))))
                     :do (put-elisp-button-here
                          ap
                          line
                          `(let ((current-window
                                   (get-buffer-window)))
                             (goto-line ,line-number
                                        (find-file-other-window
                                         ,(namestring lisp-file)))
                             (if current-window
                                 (select-window current-window)
                                 (other-window 1)))
                          :face :unspecified
                          :offset offset
                          :stream stream)
                         (terpri stream)))))
      (setf string
            (with-output-to-string (result)
              (format result "Toplevel forms:~%")
              (dolist (file (directory
                             (if (getf *buffer-context* :filename)
                                 (merge-pathnames
                                  "*.lisp"
                                  (getf *buffer-context* :filename))
                                 (merge-pathnames "*.lisp"))))
                (terpri result)
                (format result ";;; ~A~%" (file-namestring file))
                (walk file result))))
      (push `(fontify-region ,offset ,(+ offset (length string)))
            *description-properties*)
      string)))
