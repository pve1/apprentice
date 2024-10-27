;;;; Requires
;;;;   swank
;;;;   eclector
;;;;   "package"

(in-package :apprentice) cx

(defvar *Apprentice* #'describe)
(defvar *Max-description-size* 100000)
(defvar *Force-return-description* nil)
(defvar *Buffer-context* nil
  ":point :column :region :line :max-line :package :filename :locked")
(defvar *Description-stream* nil)
(defvar *previous-object* nil)
(defvar *previous-description* nil)
(defvar *description-properties* nil)
(defvar *before-describe-hook* nil)

(defgeneric Describe-with-apprentice (apprentice object stream)
  (:method (apprentice object stream)
    nil)
  (:method (apprentice (object cons) stream)
    ;; Assume plist with a "type" symbol at the head position.
    ;; E.g. (person name "John" age 37)
    (describe-tagged-list apprentice
                          (first object)
                          (rest object)
                          stream))
  (:method ((apprentice function) object stream)
    (funcall apprentice object stream)))

(defgeneric Describe-tagged-list (apprentice tag plist stream)
  (:method (apprentice tag plist stream)
    nil))

;; May return a string, a list of two elements (a string and a list
;; of text properties), :unchanged or :max-size-exceeded.
(defun return-description (object desc)
  (check-type desc (or string cons))
  (if (<= (length desc) *max-description-size*)
      (prog1 (if (and (equal *previous-object* object)
                      (equal *previous-description* desc)
                      (not *force-return-description*))
                 :unchanged
                 (if *description-properties*
                     (list desc *description-properties*)
                     desc))
        (setf *previous-object* object
              *previous-description* desc))
      :max-size-exceeded))

(defun buffer-context-property (property)
  (getf *buffer-context* property))

(defun run-hook (hook)
  (when hook
    (dolist (f hook)
      (funcall f))))

(defun funcall-maybe (package symbol &rest args)
  (let* ((sym (find-symbol (string symbol) (string package)))
         (fun (if sym
                  (fdefinition sym)
                  (error "The function ~A::~A is undefined."
                         package
                         symbol))))
    (apply fun args)))

(defun Push-description-property (property &optional where)
  (check-type where (member nil :last))
  (if (eq where :last)
      (let ((last (last *description-properties*)))
        (rplacd last (cons property nil)))
      (push property *description-properties*)))

(defun Presentation-description (presentation-id)
  (let* ((desc (with-output-to-string (s)
                 (run-hook *before-describe-hook*)
                 (describe-with-apprentice
                  *apprentice*
                  (funcall-maybe '#:swank '#:lookup-presented-object
                                 presentation-id)
                  s))))
    (when desc
      (return-description presentation-id desc))))

(defmacro with-eclector-client (client &body body)
  `(let ((*read-eval* nil)
         (eclector.base:*client* ,client))
     ,@body))

(defmethod eclector.reader:check-symbol-token ((client (eql 'default-resolve-symbol))
                                               input-stream
                                               token
                                               escape-ranges
                                               position-package-marker-1
                                               position-package-marker-2)
  (handler-case (call-next-method)
    (eclector.reader:symbol-name-must-not-end-with-package-marker ()
      (let ((symbol (make-symbol "")))
        (setf (get symbol 'package-indicator)
              (if position-package-marker-2
                  (subseq token 0 (- (length token) 2))
                  (subseq token 0 (1- (length token))))
              (get symbol 'unspecified-symbol) t)
        (throw 'symbol symbol)))))

;; Handle cl:nil specially.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+nil+)
    (defconstant +nil+ (make-symbol "NIL"))))

(defmethod eclector.reader:interpret-symbol ((client (eql 'default-resolve-symbol))
                                             input-stream
                                             package-indicator
                                             symbol-name
                                             internp)
  (flet ((anonymous-symbol (pkg)
           (let ((sym (make-symbol symbol-name)))
             (setf (get sym 'package-indicator) pkg)
             sym)))
    (cond ((eq :current package-indicator)
           (multiple-value-bind (sym exist)
               (find-symbol symbol-name *package*)
             (cond ((and exist (null sym))
                    +nil+)
                   (exist sym)
                   (t (anonymous-symbol (package-name *package*))))))
          ((eq :keyword package-indicator)
           (or (find-symbol symbol-name (find-package :keyword))
               (anonymous-symbol "KEYWORD")))
          ((find-package package-indicator)
           (or (find-symbol symbol-name package-indicator)
               (anonymous-symbol package-indicator)))
          (t (anonymous-symbol package-indicator)))))

(defgeneric Resolve-symbol (apprentice symbol-name))

(defmethod resolve-symbol (apprentice symbol-name)
  (let* ((pkg-string (buffer-context-property :package))
         (pkg (when pkg-string
                (find-package (read-from-string pkg-string))))
         (*package* (or pkg *package*)))
    (with-eclector-client 'default-resolve-symbol
      (catch 'symbol
        (eclector.reader:read-from-string symbol-name)))))

(defun Symbol-description (symbol-name)
  (check-type symbol-name string)
  (let* ((symbol (ignore-errors
                  (resolve-symbol *apprentice* symbol-name)))
         (*description-properties* nil))
    (when symbol
      (when (eq symbol +nil+)
        (setf symbol nil))
      (return-description
       symbol
       (with-output-to-string (s)
         (let ((*description-stream* s))
           (run-hook *before-describe-hook*)
           (describe-with-apprentice *apprentice*
                                     symbol
                                     s)))))))

(defgeneric Read-from-string-with-apprentice (apprentice string)
  (:method (apprentice string)
    (read-from-string string)))

(defgeneric Eval-with-apprentice (apprentice form)
  (:method (apprentice form)
    (eval form)))

(defun describe-form-separator (label &optional (width 60) newline)
  (assert (alexandria:positive-integer-p width))
  (let* ((len (length label))
         (width* (max (+ 6 len) width))
         (dashes-left
           (make-string (floor (- width* len) 2)
                        :initial-element #\-))
         (dashes-right
           (make-string (- width* (length dashes-left) len)
                        :initial-element #\-)))
    (with-output-to-string (s)
      (princ dashes-left s)
      (princ label s)
      (princ dashes-right s)
      (when newline
        (terpri s)))))

;;; Reads the package-indicator string returned by
;;; (slime-current-package).

(defgeneric Read-package-designator (apprentice package-indicator-string)
  (:method (apprentice package-indicator-string)
    (read-from-string-with-apprentice apprentice
                                      package-indicator-string)))

;;; Package-designator is the string returned by
;;; (slime-current-package). It must be READ to get the real
;;; package-designator.

(defgeneric Describe-form-with-apprentice (apprentice
                                           form-string
                                           package-designator)
  (:method (apprentice (form-string string) package-designator)
    (with-output-to-string (*standard-output*)
      (let ((*error-output* *standard-output*)
            (*description-stream* *standard-output*)
            (*description-properties* nil))
        (handler-case
            (let* ((real-package-designator
                     (read-package-designator
                      apprentice
                      package-designator))
                   (*package* (if real-package-designator
                                  (find-package real-package-designator)
                                  *package*))
                   (form (read-from-string-with-apprentice
                          apprentice
                          form-string))
                   (values))
              (princ (load-time-value
                      (describe-form-separator "*STANDARD-OUTPUT*" 60 t)))
              (setf values (multiple-value-list
                            (eval-with-apprentice apprentice
                                                  form)))
              (fresh-line)
              (princ (load-time-value
                      (describe-form-separator "VALUES" 60 t)))
              (dolist (v values)
                (prin1 v)
                (terpri))
              (values-list values))
          (error (c)
            (princ c)))))))

(defun Form-description (form-string &optional package-designator)
  (check-type form-string string)
  (return-description
   (list form-string package-designator)
   (describe-form-with-apprentice *apprentice*
                                  form-string
                                  package-designator)))

(defclass looking-at-character ()
  ((preceding-char :initarg :preceding-char
                   :accessor preceding-char
                   :initform nil)
   (following-char :initarg :following-char
                   :accessor following-char
                   :initform nil)))

(defmethod print-object ((l looking-at-character) stream)
  (print-unreadable-object (l stream :type t)
    (format stream "~S ~S"
            (preceding-char l)
            (following-char l)))
  l)

(defun Character-description (preceding-char following-char)
  (let* ((*description-properties* nil))
    (return-description
     (list 'looking-at preceding-char following-char)
     (with-output-to-string (s)
       (let ((*description-stream* s))
         (run-hook *before-describe-hook*)
         (describe-with-apprentice
          *apprentice*
          (make-instance 'looking-at-character
            :following-char (when following-char
                              (aref following-char 0))
            :preceding-char (when preceding-char
                              (aref preceding-char 0)))
          s))))))

