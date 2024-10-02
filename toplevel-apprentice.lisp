;;;; Requires
;;;;   cl-ppcre
;;;;   "slime-apprentice"
;;;;   "buttons"

(in-package :slime-apprentice) cx

(defclass Toplevel-apprentice (caching-apprentice)
  ((busy-result :initarg :busy-result
                :accessor busy-result
                :initform "Toplevel forms: ...")))

;; Improve this
(defmethod apprentice-same-input-as-last-time-p ((ap toplevel-apprentice)
                                                 (input looking-at-character))
  nil)

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
  ((sort-lines-p :initarg :sort-lines-p
                 :accessor sort-lines-p
                 :initform nil)
   (file-selection-mode :initarg :file-selection-mode
                        :accessor file-selection-mode
                        :initform :file) ; :directory
   (ignore-line-regexp :initarg :ignore-line-regexp
                       :accessor ignore-line-regexp
                       :initform "^ *$|^;|^\\(in-package")))

(defmethod initialize-instance :after ((w wide-toplevel-apprentice)
                                       &key ignore-line-regexp)
  (setf *toplevel-apprentice* w)
  (when (stringp ignore-line-regexp)
    (setf (ignore-line-regexp w)
          (cl-ppcre:create-scanner ignore-line-regexp))))

;; Singleton
(defvar *toplevel-apprentice* nil)

;; Hack
(defmethod apprentice-update ((ap wide-toplevel-apprentice)
                              object)
  (let ((offset (file-position *standard-output*))
        (ignore-line-scanner (ignore-line-regexp ap))
        (string))
    (flet ((walk (lisp-file stream)
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
                 (if (getf *buffer-context* :filename)
                     (merge-pathnames
                      "*.lisp"
                      (getf *buffer-context* :filename))
                     (merge-pathnames "*.lisp"))))
               (:file
                (list (getf *buffer-context* :filename))))))
      (setf string
            (with-output-to-string (result)
              (format result "Toplevel forms: ")
              (put-lisp-button-here
               ap
               "FILE"
               '(setf (file-selection-mode *toplevel-apprentice*)
                 :file)
               :stream result
               :offset offset
               :redisplay t)
              (princ " " result)
              (put-lisp-button-here
               ap
               "DIR"
               '(setf (file-selection-mode *toplevel-apprentice*)
                 :directory)
               :stream result
               :offset offset
               :redisplay t)
              (princ " " result)
              (put-lisp-button-here
               ap
               "SORT"
               '(setf (sort-lines-p *toplevel-apprentice*)
                 (not (sort-lines-p *toplevel-apprentice*)))
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
                         ap
                         line
                         `(let ((current-window
                                  (get-buffer-window)))
                            (goto-line ,line-number
                                       (find-file-other-window
                                        ,(namestring file)))
                            (if current-window
                                (select-window current-window)
                                (other-window 1)))
                         :face :unspecified
                         :offset offset
                         :stream result)
                        (terpri result))))
                  (dolist (file (select-files))
                    (terpri result)
                    (format result ";;; ~A~%" (file-namestring file))
                    (walk file result)))))
      (push `(fontify-region ,offset ,(+ offset (length string)))
            *description-properties*)
      string)))

;; Note: can't filter lines here, as buttons have already been added.
(defmethod toplevel-apprentice-sort-lines (apprentice string)
  (with-input-from-string (s string)
    (let ((lines (loop :for line = (read-line s nil s)
                       :until (eq line s)
                       :collect line)))
      (setf lines (sort lines #'string<))
      (with-output-to-string (s2)
        (dolist (l lines)
          (princ l s2)
          (terpri s2))))))
