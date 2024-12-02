;;;; Requires
;;;;   cl-ppcre
;;;;   alexandria
;;;;   "apprentice"
;;;;   "caching-apprentice"
;;;;   "buttons"
;;;;   "symbols"
;;;;   "emacs"

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
   (line-display-mode :initarg :line-display-mode
                      :accessor line-display-mode
                      :initform (list :normal
                                      :names
                                      :uninterned))
   (recenter :initarg :recenter
             :accessor recenter
             :initform nil)
   (file-selection-mode :initarg :file-selection-mode
                        :accessor file-selection-mode
                        :initform :file) ; :directory
   (ignore-line-regexp :initarg :ignore-line-regexp
                       :accessor ignore-line-regexp
                       :initform "^ *$|^;|^\\(in-package")
   (symbols :initarg :symbols
            :accessor symbols
            :initform nil)))

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

(defun copy-toplevel-line (toplevel-line)
  (make-instance 'toplevel-line
    :file (file toplevel-line)
    :line-number (line-number toplevel-line)
    :line (line toplevel-line)))

(defmethod toplevel-line-button (apprentice line)
  (put-elisp-button-here
   apprentice (line line) nil
   :name 'toplevel-apprentice-button
   :arguments (list (line-number line)
                    (namestring (file line)))
   :face :unspecified
   :skippable t))

(defun extract-toplevel-name (toplevel-line)
  (multiple-value-bind (match sub)
      (cl-ppcre:scan-to-strings
       "^\\(\\w+ ([^()]+?)[ \\n]" ; Investigate: (\\w+?) doesn't work
       toplevel-line)
    (declare (ignore match))
    (when sub
      (aref sub 0))))

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

(defmethod button-pressed ((apprentice wide-toplevel-apprentice)
                           (name (eql 'display))
                           &rest rest)
  (setf (line-display-mode apprentice)
        (alexandria:rotate (line-display-mode apprentice) -1))
  (emacs-message
   (case (car (line-display-mode apprentice))
     (:normal "Lines")
     (:names "Present symbols")
     (:uninterned "Present symbols as uninterned"))))

(defmethod button-pressed ((apprentice wide-toplevel-apprentice)
                           (name (eql 'export))
                           &rest rest)
  (export (symbols apprentice) *package*)
  (emacs-message
   (format nil "Exported ~A symbols from ~A."
           (length (symbols apprentice))
           *package*)))

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
    (princ " ")
    ;; Todo: show only present symbols
    (put-lisp-button-here
     ap
     "[DISP]"
     '(button-pressed *button-apprentice* 'display)
     :redisplay t)
    (when (member (car (line-display-mode ap))
                  '(:names :uninterned))
      (princ " ")
      (put-lisp-button-here
       ap
       "[EXPORT]"
       '(button-pressed *button-apprentice* 'export)))
    (terpri)
    (terpri)
    ;; Display
    (let ((begin (file-position *standard-output*)))
      (setf (symbols ap) nil) ; for export
      (flet ((presentp (line)
               (alexandria:when-let*
                   ((name (extract-toplevel-name
                           (line line)))
                    (uppercase (string-upcase name))
                    (present (symbol-present-p uppercase)))
                 (push (find-symbol uppercase) (symbols ap))
                 name)))
        (cond ((eq :names (car (line-display-mode ap)))
               (setf lines (mapcar #'copy-toplevel-line lines))
               (dolist (line lines)
                 (setf (line line) (presentp line))))
              ((eq :uninterned (car (line-display-mode ap)))
               (setf lines (mapcar #'copy-toplevel-line lines))
               (dolist (line lines)
                 (setf (line line)
                       (alexandria:when-let*
                           ((name (presentp line)))
                         (concatenate 'string "#:" name)))))))
      ;; line-display-mode may have resulted in NIL lines
      (when (not (eq :normal (car (line-display-mode ap))))
        (setf lines (remove-if (lambda (x) (null (line x)))
                               lines))
        ;; Use slime-current-package instead?
        (unless lines
          (format t "No present symbols. Remember to synchronize *PACKAGE*.~%"))
        (when (sort-lines-p ap)
          ;; Todo: use loop here, since the list is sorted.
          (setf lines (remove-duplicates
                       lines :key #'line
                             :test #'string=))))
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
