(in-package :slime-apprentice)

(unless (find-package "SLIME-APPRENTICE-READ")
  (make-package "SLIME-APPRENTICE-READ" :use nil))

(defvar *apprentice* nil)
(defvar *max-description-size* 100000)
(defvar *force-return-description* nil)
(defvar *previous-object* nil)
(defvar *previous-description* nil)

(defgeneric describe-with-apprentice (apprentice object stream)
  (:method (apprentice object stream)
    (describe object stream)))

;; May return a string, :unchanged or :max-size-exceeded.
(defun return-description (object desc)
  (check-type desc string)
  (if (<= (length desc) *max-description-size*)
      (prog1 (if (and (equal *previous-object* object) ; may be a string if unknown symbol
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

(defun presentation-description (presentation-id)
  (let* ((desc (with-output-to-string (s)
                 (describe-with-apprentice
                  *apprentice*
                  (funcall-maybe '#:swank '#:lookup-presented-object
                                 presentation-id)
                  s))))
    (when desc
      (return-description presentation-id desc))))

(defclass eclector-client () ())

(defparameter *eclector-client* (make-instance 'eclector-client))

(defmethod eclector.reader:interpret-symbol ((client eclector-client)
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

(defun read-from-string-with-client (string)
  (let ((eclector.base:*client* *eclector-client*))
    (eclector.reader:read-from-string string)))

(defmethod resolve-symbol (apprentice symbol-name)
  (read-from-string-with-client symbol-name))

(defun symbol-description (symbol-name)
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
