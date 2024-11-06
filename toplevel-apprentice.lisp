;;;; Requires
;;;;   cl-ppcre
;;;;   alexandria
;;;;   "apprentice"
;;;;   "caching-apprentice"
;;;;   "buttons"

(in-package :apprentice) cx

(defclass Toplevel-apprentice (caching-apprentice)
  ((busy-result :initarg :busy-result
                :accessor busy-result
                :initform "Toplevel forms: ...")))

(defmethod apprentice-same-input-as-last-time-p ((ap toplevel-apprentice)
                                                 (input looking-at-character))
  nil)

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
                                 (eql #\( (alexandria:first-elt line)))
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

(defclass Wide-toplevel-apprentice (caching-apprentice)
  ((busy-result :initarg :busy-result
                :accessor busy-result
                :initform "Toplevel forms: ...")
   (sort-lines-p :initarg :sort-lines-p
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

(defmethod busy-result ((ap wide-toplevel-apprentice))
  (last-result ap))

(defclass toplevel-line ()
  ((line :initarg :line
         :accessor line
         :initform nil)
   (line-number :initarg :line-number
                :accessor line-number
                :initform nil)
   (file :initarg :file
         :accessor file
         :initform nil))
  (:documentation ""))

(defmethod toplevel-line-button (apprentice line)
  (put-elisp-button-here
   apprentice (line line) nil
   :name 'toplevel-apprentice-button
   :arguments (list (line-number line)
                    (namestring (file line)))
   :face :unspecified
   :skippable t))

;; Scans the files and returns a list of toplevel-line instancs.
;; Should not put buttons.
(defmethod apprentice-update ((ap wide-toplevel-apprentice) input)
  (let* ((ignore-line-scanner (ignore-line-regexp ap)))
    (flet ((collect-lines (lisp-file)
             (alexandria:with-input-from-file (s lisp-file)
               (loop :for line = (read-line s nil s)
                     :for line-number :from 1
                     :until (eq line s)
                     :when (and (< 0 (length line))
                                (eql #\( (alexandria:first-elt line))
                                (not (cl-ppcre:scan
                                      ignore-line-scanner
                                      line)))
                     :collect (make-instance 'toplevel-line
                                :file lisp-file
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
      (let ((lines (loop :for file
                         :in (select-files)
                         :append (collect-lines file))))
        (if (sort-lines-p ap)
            (sort lines #'string< :key #'line)
            lines)))))

(defmethod describe-with-apprentice ((ap wide-toplevel-apprentice)
                                     object
                                     stream)
  (let* ((*standard-output* stream)
         (lines (apprentice-update-maybe ap object)))
    (when (stringp lines) ; busy
      (princ lines)
      (return-from describe-with-apprentice t))
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
    (format t "Toplevel forms: ")
    (put-lisp-button-here
     ap
     "[FILE]"
     '(setf (file-selection-mode *button-apprentice*)
       :file)
     :redisplay t)
    (princ " ")
    (put-lisp-button-here
     ap
     "[DIR]"
     '(setf (file-selection-mode *button-apprentice*)
       :directory)
     :redisplay t)
    (princ " ")
    (put-lisp-button-here
     ap
     "[SORT]"
     '(setf (sort-lines-p *button-apprentice*)
       (not (sort-lines-p *button-apprentice*)))
     :redisplay t)
    (terpri)
    (terpri)
    (let ((begin (file-position *standard-output*)))
      (loop :with last-file
            :with sort = (sort-lines-p ap)
            :for line :in lines
            :for file = (file line)
            :when (and (not (equal last-file file))
                       (not (and sort (eq :directory
                                          (file-selection-mode
                                           ap)))))
            :do (format t "~:[~;~%~];;; ~A~%"
                        last-file
                        (file-namestring file))
                (setf last-file file)
            :do (toplevel-line-button ap line)
                (terpri))
      (push-description-property
       `(fontify-region ,begin ,(file-position *standard-output*))))))
