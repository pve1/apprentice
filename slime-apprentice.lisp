;;;; Requires
;;;;   swank
;;;;   eclector
;;;;   "package"

(in-package :slime-apprentice) cx

(defvar *Apprentice* nil)
(defvar *Max-description-size* 100000)
(defvar *Force-return-description* nil)
(defvar *Buffer-context* nil)
(defvar *previous-object* nil)
(defvar *previous-description* nil)

(defgeneric Describe-with-apprentice (apprentice object stream)
  (:method (apprentice object stream)
    (describe object stream)
    t)
  (:method ((apprentice function) object stream)
    (funcall apprentice object stream)))

;; May return a string, :unchanged or :max-size-exceeded.
(defun return-description (object desc)
  (check-type desc string)
  (if (<= (length desc) *max-description-size*)
      (prog1 (if (and (equal *previous-object* object)
                      (equal *previous-description* desc)
                      (not *force-return-description*))
                 :unchanged
                 desc)
        (setf *previous-object* object
              *previous-description* desc))
      :max-size-exceeded))

(defun funcall-maybe (package symbol &rest args)
  (let* ((sym (find-symbol (string symbol) (string package)))
         (fun (if sym
                  (fdefinition sym)
                  (error "The function ~A::~A is undefined."
                         package
                         symbol))))
    (apply fun args)))

(defun Presentation-description (presentation-id)
  (let* ((desc (with-output-to-string (s)
                 (describe-with-apprentice
                  *apprentice*
                  (funcall-maybe '#:swank '#:lookup-presented-object
                                 presentation-id)
                  s))))
    (when desc
      (return-description presentation-id desc))))

(defmacro with-eclector-client (client &body body)
  `(let((*read-eval* nil)
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
           (or (find-symbol symbol-name *package*)
               (anonymous-symbol (package-name *package*))))
          ((eq :keyword package-indicator)
           (or (find-symbol symbol-name (find-package :keyword))
               (anonymous-symbol "KEYWORD")))
          ((find-package package-indicator)
           (or (find-symbol symbol-name package-indicator)
               (anonymous-symbol package-indicator)))
          (t (anonymous-symbol package-indicator)))))

(defgeneric Resolve-symbol (apprentice symbol-name))

(defmethod resolve-symbol (apprentice symbol-name)
  (with-eclector-client 'default-resolve-symbol
    (catch 'symbol
      (eclector.reader:read-from-string symbol-name))))

(defun Symbol-description (symbol-name)
  (check-type symbol-name string)
  (let* ((symbol (ignore-errors
                  (resolve-symbol *apprentice* symbol-name))))
    (when symbol
      (return-description
       symbol
       (with-output-to-string (s)
         (describe-with-apprentice *apprentice*
                                   symbol
                                   s))))))

(defgeneric Read-from-string-with-apprentice (apprentice string)
  (:method (apprentice string)
    (read-from-string string)))

(defgeneric Eval-with-apprentice (apprentice form)
  (:method (apprentice form)
    (eval form)))

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
      (let ((*error-output* *standard-output*))
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
                   (values (multiple-value-list
                            (eval-with-apprentice apprentice
                                                  form))))
              (format t "~&~%------------------------------------------------------------~%")
              (dolist (v values)
                (print v))
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

