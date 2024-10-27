;;;; Requires
;;;;   cl-ppcre
;;;;   "apprentice"
;;;;   "buttons"

(in-package :apprentice) cx

(defclass Toplevel-apprentice (caching-apprentice)
  ())

;; Improve this
(defmethod apprentice-same-input-as-last-time-p ((ap toplevel-apprentice)
                                                 (input looking-at-character))
  nil)

(defmethod busy-result ((ap toplevel-apprentice))
  (last-result ap))

(defmethod last-result :around ((ap toplevel-apprentice))
  (let ((offset (file-position *description-stream*))
        (result (call-next-method)))
    (when result
      (push-description-property
       `(fontify-region ,offset ,(+ offset (length result)))
       :last))
    result))

(defmethod apprentice-update ((ap toplevel-apprentice) object)
  (let ((current-file (buffer-context-property :filename))
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
                                 (eql #\( (alexandria:first-elt line))
                                 #+n
                                 (not (member (alexandria:first-elt line)
                                              '(#\Space #\Tab #\;))))
                      :do (put-elisp-button-here
                           ap line
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
      (push-description-property
       `(fontify-region ,offset ,(+ offset (length string))))
      string)))

(defclass Wide-toplevel-apprentice (toplevel-apprentice)
  ((sort-lines-p :initarg :sort-lines-p
                 :accessor sort-lines-p
                 :initform nil)
   (recenter :initarg :recenter
             :accessor recenter
             :initform nil)
   (file-selection-mode :initarg :file-selection-mode
                        :accessor file-selection-mode
                        :initform :file) ; :directory
   (ignore-line-regexp :initarg :ignore-line-regexp
                       :accessor ignore-line-regexp
                       :initform "^ *$|^;|^\\(in-package")))

(defmethod initialize-instance :after ((w wide-toplevel-apprentice)
                                       &key ignore-line-regexp)
  (check-type (recenter w) (or null integer))
  (when (stringp ignore-line-regexp)
    (setf (ignore-line-regexp w)
          (cl-ppcre:create-scanner ignore-line-regexp))))

(defmethod apprentice-update ((ap wide-toplevel-apprentice)
                              object)
  (let ((offset (file-position *standard-output*))
        (ignore-line-scanner (ignore-line-regexp ap))
        (string))
    (create-ephemeral-elisp-function
     ap 'toplevel-apprentice-button
     `(lambda (line-number file)
       (let ((current-window (get-buffer-window)))
         (goto-line line-number
                    (find-file-other-window
                     file))
         (recenter-top-bottom ,(recenter ap))
         (if current-window
             (select-window current-window)
             (other-window 1)))))
    (flet ((walk (lisp-file stream)
             (alexandria:with-input-from-file (s lisp-file)
               (loop :for line = (read-line s nil s)
                     :for line-number :from 1
                     :until (eq line s)
                     :when (and (< 0 (length line))
                                (eql #\( (alexandria:first-elt line))
                                #+n
                                (not (member (alexandria:first-elt line)
                                             '(#\Space #\Tab #\;)))
                                (not (cl-ppcre:scan
                                      ignore-line-scanner
                                      line)))
                     :do (put-elisp-button-here
                          ap line nil
                          :name
                          'toplevel-apprentice-button
                          :arguments (list line-number
                                           (namestring lisp-file))
                          :face :unspecified
                          :offset offset
                          :stream stream
                          :skippable t)
                         (terpri stream))))
           (collect-lines (lisp-file)
             (alexandria:with-input-from-file (s lisp-file)
               (loop :for line = (read-line s nil s)
                     :for line-number :from 1
                     :until (eq line s)
                     :when (and (< 0 (length line))
                                (not (member (alexandria:first-elt line)
                                             '(#\Space #\Tab #\;)))
                                (not (cl-ppcre:scan
                                      ignore-line-scanner
                                      line)))
                     :collect (list :file lisp-file
                                    :line line
                                    :line-number line-number))))
           (select-files ()
             (case (file-selection-mode ap)
               (:directory
                (directory
                 (if (buffer-context-property :filename)
                     (merge-pathnames
                      "*.lisp"
                      (buffer-context-property :filename))
                     (merge-pathnames "*.lisp"))))
               (:file
                (alexandria:when-let* ((file (buffer-context-property
                                                   :filename))
                                       (exist (probe-file file)))
                  (list file))))))
      (setf string
            (with-output-to-string (result)
              (format result "Toplevel forms: ")
              (put-lisp-button-here
               ap
               "[FILE]"
               '(setf (file-selection-mode *button-apprentice*)
                 :file)
               :stream result
               :offset offset
               :redisplay t)
              (princ " " result)
              (put-lisp-button-here
               ap
               "[DIR]"
               '(setf (file-selection-mode *button-apprentice*)
                 :directory)
               :stream result
               :offset offset
               :redisplay t)
              (princ " " result)
              (put-lisp-button-here
               ap
               "[SORT]"
               '(setf (sort-lines-p *button-apprentice*)
                 (not (sort-lines-p *button-apprentice*)))
               :stream result
               :offset offset
               :redisplay t)
              (terpri result)
              (if (sort-lines-p ap)
                  (let ((lines (loop :for file
                                     :in (select-files)
                                     :append (collect-lines file))))
                    (setf lines (sort lines
                                      #'string<
                                      :key (lambda (x) (getf x :line))))
                    (terpri result)
                    (when (eq :file (file-selection-mode ap))
                      (format result ";;; ~A~%"
                              (file-namestring
                               (getf (first lines) :file))))
                    (dolist (line lines)
                      (destructuring-bind (&key line line-number file)
                          line
                        (put-elisp-button-here
                         ap line nil
                         :name 'toplevel-apprentice-button
                         :arguments (list line-number
                                          (namestring file))
                         :face :unspecified
                         :offset offset
                         :stream result
                         :skippable t)
                        (terpri result))))
                  (dolist (file (select-files))
                    (terpri result)
                    (format result ";;; ~A~%" (file-namestring file))
                    (walk file result)))))
      (push-description-property
       `(fontify-region ,offset ,(+ offset (length string))))
      string)))
