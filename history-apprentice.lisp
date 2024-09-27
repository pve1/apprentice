;;;; Requires
;;;;   fset
;;;;   slime-apprentice
;;;;   slime-apprentice/buttons

(in-package :slime-apprentice) cx

(defclass History-apprentice ()
  ((history :initarg :history
            :accessor history
            :initform nil)
   (max-files :initarg :max-files
              :accessor max-files
              :initform 10)
   (max-locations :initarg :max-locations
                  :accessor max-locations
                  :initform 5)
   (file-prefix-length :initarg :file-prefix-length
                       :accessor file-prefix-length
                       :initform 8)
   (symbol-prefix-length :initarg :symbol-prefix-length
                         :accessor symbol-prefix-length
                         :initform 8)))

(defmethod initialize-instance :after ((ap history-apprentice) &key)
  (unless (history ap)
    (setf (history ap) (fset:empty-seq))))

;; file1 SYM1 SYM2 SYM3 SYM4
;; file2 SYM1 SYM2 SYM3 SYM4
;; file3 SYM1 SYM2 SYM3 SYM4
;; ...

(defun history-chop-and-pad (string length)
  (let ((chopped (make-string length :initial-element #\Space)))
    (loop :for i :from 0
          :for c :across string
          :for c2 :across chopped
          :do (setf (aref chopped i) c))
    chopped))

(defclass history-entry ()
  ((symbol :initarg :symbol
           :accessor the-symbol          
           :initform nil)
   (file :initarg :file
         :accessor file
         :initform nil)
   (point :initarg :point
          :accessor point
          :initform nil)))

(defmethod describe-with-apprentice ((ap history-apprentice) 
                                     (object symbol)
                                     stream)
  (let* ((history (history ap))) ; (filename . location seq)
    (destructuring-bind (&key filename point &allow-other-keys)
        *buffer-context*
      ;; Update
      
      ;; TODO: bump file on update
      (when filename
        (let* ((existing-entry (fset:find filename history :key 'car))            
               (symbols (cdr existing-entry)))
          (cond (existing-entry
                 (unless (eql (the-symbol (fset:first symbols))
                              object)
                   (setf (cdr existing-entry)
                         (if (< (fset:size symbols)
                                (max-locations ap))
                             (fset:with-first symbols
                               (make-instance 'history-entry 
                                 :symbol object
                                 :file filename
                                 :point point))
                             (fset:with-first (fset:less-last symbols)
                               (make-instance 'history-entry 
                                 :symbol object
                                 :file filename
                                 :point point))))))
                (t 
                 (setf (history ap)
                       (fset:with-first (history ap)
                         (cons filename
                               (let ((seq (fset:empty-seq)))
                                 (dotimes (n (1- (max-locations ap)))
                                   (setf seq (fset:with-first seq
                                               (make-instance 'history-entry 
                                                 :symbol (make-symbol "")
                                                 :file nil
                                                 :point nil))))
                                 (fset:with-first seq
                                   (make-instance 'history-entry 
                                     :symbol object
                                     :file filename
                                     :point point))))))))))
      (when history
        (format stream "History:~%~%")
        (fset:do-seq (pair (history ap))
          (format stream "~A: " (history-chop-and-pad 
                                 (pathname-name (car pair))
                                 (file-prefix-length ap)))
          (fset:do-seq (sym (cdr pair))
            (if (null (point sym))
                (princ (history-chop-and-pad
                        (symbol-name (the-symbol sym))
                        (symbol-prefix-length ap))
                       stream)
                (put-elisp-button-here 
                 ap 
                 (history-chop-and-pad
                  (symbol-name (the-symbol sym))
                  (symbol-prefix-length ap))
                 `(progn (find-file-other-window ,(file sym))
                         (condition-case
                          nil
                          (goto-char ,(point sym))
                          (error (beginning-of-buffer)))
                         (other-window 1))
                 :stream stream))
            (princ " | " stream))
          (terpri stream))
        t))))

