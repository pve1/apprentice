;;;; Requires
;;;;   "symbols"

(in-package :apprentice) cx

;; WIP

(defvar *Locate-symbols-client*)
(defvar *Locate-symbols-token-start*)
(defvar *Locate-symbols-package-indicator*)
(defvar *Locate-symbols-package-marker-1*)
(defvar *Locate-symbols-package-marker-2*)

(defclass locate-symbols-client ()
  ((interpret-symbol-function
    :initarg :interpret-symbol-function
    :accessor interpret-symbol-function
    :initform (constantly nil))
   (token-start :initarg :token-start
                :accessor token-start
                :initform 0)
   (collected-symbols :initarg :collected-symbols
                      :accessor collected-symbols
                      :initform nil))
  (:documentation ""))

(defmethod eclector.reader:evaluate-expression
    ((client locate-symbols-client) expression)
  expression)

(defmethod collect ((client locate-symbols-client)
                    (object symbol))
  (push object (collected-symbols client)))

(defmethod eclector.reader:read-token :before
    ((client locate-symbols-client)
     input-stream
     eof-error-p
     eof-value)
  (setf (token-start client)
        (file-position input-stream)))

(defmethod eclector.reader:read-token :around
    ((client locate-symbols-client)
     input-stream
     eof-error-p
     eof-value)
  (let ((*locate-symbols-token-start*
          (file-position input-stream)))
    (call-next-method)))

(defmethod eclector.reader:interpret-symbol-token
    ((client locate-symbols-client)
     input-stream
     token
     position-package-marker-1
     position-package-marker-2)
  (let ((*locate-symbols-package-marker-1*
          position-package-marker-1)
        (*locate-symbols-package-marker-2*
          position-package-marker-2))
    (call-next-method)))

(defmethod eclector.reader:interpret-symbol
    ((client locate-symbols-client)
     input-stream
     package-indicator
     symbol-name
     internp)
  (let ((*locate-symbols-client* client))
    (funcall (interpret-symbol-function client)
             symbol-name
             package-indicator)))

;; Whoever uses this should bind *package* first.
(defun In-package-form-fn (form)
  (when (and (listp form)
             (eq (car form) 'in-package))
    (setf *package* (find-package (cadr form)))))

(defun Call-with-forms (file fn)
  (alexandria:with-input-from-file (f file)
    (loop :for form = (eclector.reader:read f nil f)
          :until (eq form f)
          :do (funcall fn form))))

(defun Call-with-symbol-names (file symbol-fn
                               &key (form-fn (constantly nil)))
  (with-eclector-client (make-instance 'locate-symbols-client
                          :interpret-symbol-function symbol-fn)
    (call-with-forms file form-fn)))

(defun Call-with-symbols (file symbol-fn
                          &key (form-fn (constantly nil))
                               (on-missing-symbol :skip))
  (check-type on-missing-symbol (member null :skip :error))
  (let ((interpret
          (lambda (name package-indicator)
            (if (null package-indicator)
                (funcall symbol-fn (make-symbol name))
                (let ((package (case package-indicator
                                 (:current *package*)
                                 (:keyword (find-package :keyword))
                                 (t (find-package package-indicator)))))
                  (multiple-value-bind (symbol status)
                      (find-symbol name package)
                    (when (and (null status)
                               (eq on-missing-symbol :error))
                      (error "Symbol ~A not found in ~A."
                             name package))
                    (when status
                      (let ((*locate-symbols-package-indicator*
                              package-indicator))
                        (funcall symbol-fn symbol)))))))))
    (with-eclector-client (make-instance 'locate-symbols-client
                            :interpret-symbol-function interpret)
      (call-with-forms file form-fn))))

(defun Locate-symbols (file predicate)
  (let ((locations ())
        (*package* *package*))
    (call-with-symbols
     file
     (lambda (symbol)
       (when (funcall predicate symbol)
         (push (list *locate-symbols-token-start*
                     symbol)
               locations)))
     :form-fn 'in-package-form-fn)
    ;; Reverse order is intentional because it makes inserting easier.
    locations))

(defun Locate-symbol-names (file predicate)
  (let ((locations ())
        (*package* *package*))
    (call-with-symbol-names
     file
     (lambda (name package-indicator)
       (when (funcall predicate name package-indicator)
         (push (list *locate-symbols-token-start*
                     name
                     package-indicator
                     *locate-symbols-package-marker-1*
                     *locate-symbols-package-marker-2*)
               locations)))
     :form-fn 'in-package-form-fn)
    ;; Reverse order is intentional because it makes inserting easier.
    locations))

(defun locate-unqualified-symbols-from-package (file package-name)
  (let ((package (find-package package-name)))
    (locate-symbols
     file
     (lambda (symbol)
       (when (eq :current *locate-symbols-package-indicator*)
         (eq package (symbol-package symbol)))))))

(defun locate-qualified-symbols-from-package (file package-name &key exactly)
  (let* ((package (find-package package-name))
         (primary-name (package-name package)))
    (locate-symbols
     file
     (lambda (symbol)
       (and (not (eq :current
                     *locate-symbols-package-indicator*))
            (equal package (symbol-package symbol)))))))

(defun locate-qualified-symbol (file symbol)
  (let ((package-name (package-name
                       (symbol-package symbol))))
    (locate-symbols
     file
     (lambda (sym)
       (and (eq symbol sym)
            (equal package-name
                   *locate-symbols-package-indicator*))))))

(defun locate-unqualified-symbol (file symbol)
  (locate-symbols
   file
   (lambda (sym)
     (and (eq symbol sym)
          (eq :current
              *locate-symbols-package-indicator*)))))

(defun symbol-package-markers-loc (symbol)
  (multiple-value-bind (symbol status)
      (find-symbol (string symbol)
                   (symbol-package symbol))
    (case status
      (:external ":")
      (:inherited "::")
      (:internal "::"))))

(defun symbol-package-prefix-loc (symbol)
  (let ((markers (symbol-package-prefix-loc
                  symbol)))
    (concatenate 'string
                 (string-downcase
                  (package-name
                   (symbol-package symbol)))
                 markers)))

;; An edit can be (1234 :insert "foo:") or (1234 :delete 3).

(defun emacs-perform-edits (file edits &optional (reindent t))
  (swank:eval-in-emacs
   `(cl-flet
     ((del (n) (delete-forward-char n))
      (ins (str) (insert str)))
     (save-excursion
      (with-current-buffer (find-file-noselect ,file)
        (dolist (edit ',edits)
          (cl-destructuring-bind
           (pos operation &rest args) edit
           (goto-char (1+ pos))
           (cl-case operation
                    (:insert (apply #'ins args))
                    (:delete (apply #'del args)))))
        ,(when reindent
           `(indent-region (point-min) (point-max))))))))

(defun emacs-insert-strings (file position-string-pairs
                             &optional (reindent t))
  (emacs-perform-edits
   file
   (mapcar (lambda (x)
             (destructuring-bind (pos string) x
               (list pos :insert string)))
          position-string-pairs)
   reindent))

;;; TODO: Design the compute-edits functionality properly

(defun compute-edits-for-use-package (file used-package-designator
                                      &optional (package *package*))
  (let* ((used-package (find-package used-package-designator))
         (locs (locate-symbol-names
                file
                (lambda (name package-indicator)
                  (and (stringp package-indicator)
                       (eq used-package (find-package
                                         package-indicator)))))))
    (mapcar (lambda (loc)
              (destructuring-bind (pos name pkgi pmark1 pmark2)
                  loc
                (list pos :delete (+ (length pkgi)
                                     (if pmark2
                                         2
                                         1)))))
            locs)))

(defun find-best (list predicate &optional (key #'identity))
  "PREDICATE should return non-nil if its first argument is 'better'
than its second argument."
  (let ((best (first list)))
    (loop :for item :in (rest list)
          :when (funcall predicate
                         (funcall key item)
                         (funcall key best))
          :do (setf best item))
    best))

(defun compute-edits-for-unuse-package (file unused-package-designator
                                        &optional (package *package*))
  (let* ((unused-package (find-package unused-package-designator))
         (shortest-name
           (find-best (list* (package-name unused-package)
                             (package-nicknames unused-package))
                      (lambda (x y)
                        (< (length x)
                           (length y)))))
         (prefix (concatenate 'string
                              (string-downcase shortest-name)
                              ":"))
         (locs (locate-symbol-names
                file
                (lambda (name package-indicator)
                  (and (eq *package* package)
                       (eq :current package-indicator)
                       (multiple-value-bind (symbol state)
                           (find-symbol name package)
                         (and (eq unused-package
                                  (symbol-package symbol))
                              (eq state :inherited))))))))
    (mapcar (lambda (loc)
              (destructuring-bind (pos name pkgi pmark1 pmark2)
                  loc
                (list pos :insert prefix)))
            locs)))

(defun compute-edits-for-import-symbol (file imported-symbol
                                        &optional (package *package*))
  (let* ((locs (locate-symbol-names
                file
                (lambda (name package-indicator)
                  (and (eq *package* package)
                       (stringp package-indicator)
                       (multiple-value-bind (symbol state)
                           (find-symbol name package-indicator)
                         (eq symbol imported-symbol)))))))
    (mapcar (lambda (loc)
              (destructuring-bind (pos name pkgi pmark1 pmark2)
                  loc
                (list pos :delete (+ (length pkgi)
                                     (if pmark2
                                         2
                                         1)))))
            locs)))

(defun compute-edits-for-unintern-symbol (file uninterned-symbol
                                          &optional (package *package*))
  (let* ((symbol-package (symbol-package uninterned-symbol))
         (shortest-name
           (find-best (list* (package-name symbol-package)
                             (package-nicknames symbol-package))
                      (lambda (x y)
                        (< (length x)
                           (length y)))))
         (prefix (concatenate 'string
                              (string-downcase shortest-name)
                              (symbol-package-markers-loc
                               uninterned-symbol)))
         (locs (locate-symbol-names
                file
                (lambda (name package-indicator)
                  (and (eq *package* package)
                       (eq :current package-indicator)
                       (multiple-value-bind (symbol state)
                           (find-symbol name package)
                         (eq symbol uninterned-symbol)))))))
    (mapcar (lambda (loc)
              (destructuring-bind (pos name pkgi pmark1 pmark2)
                  loc
                (list pos :insert prefix)))
            locs)))
