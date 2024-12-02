;;;; Requires
;;;;   "symbols"
;;;;   "emacs"

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

(defmethod Call-with-forms ((stream stream) fn)
  (loop :for form = (eclector.reader:read
                     stream nil stream)
        :until (eq form stream)
        :do (funcall fn form)))

(defmethod Call-with-forms ((file string) fn)
  (alexandria:with-input-from-file (f file)
    (call-with-forms f fn)))

;; Symbol-fn: symbol-name, package-indicator -> ...
(defun Call-with-symbol-names (file symbol-fn
                               &key (form-fn (constantly nil)))
  (with-eclector-client (make-instance 'locate-symbols-client
                          :interpret-symbol-function symbol-fn)
    (call-with-forms file form-fn)))

;; Symbol-fn: symbol -> ...
;; Form-fn should be something like in-package-form-fn
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
                     (cond ((and *locate-symbols-package-marker-1*
                                 *locate-symbols-package-marker-2*)
                            "::")
                           (*locate-symbols-package-marker-1*
                            ":")
                           (t "")))
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

;; Utils

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

(defun shortest-package-name-loc (package)
  (find-best (list* (package-name package)
                    (package-nicknames package))
             (lambda (x y)
               (< (length x)
                  (length y)))))

(defun symbol-package-markers-loc (symbol &optional package)
  (let* ((package* (or package (symbol-package symbol)))
         (status (symbol-status symbol package*)))
    (case status
      (:external ":")
      (:inherited "::")
      (:internal "::"))))

(defun symbol-package-prefix-loc (symbol &optional package)
  (let* ((package* (or package (symbol-package symbol)))
         (markers (symbol-package-markers-loc symbol
                                              package*)))
    (concatenate 'string
                 (string-downcase
                  (package-name (or package
                                    (symbol-package symbol))))
                 markers)))

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
              (destructuring-bind (pos name pkgi pkgmarker)
                  loc
                (list pos :delete (+ (length pkgi)
                                     (length pkgmarker)))))
            locs)))

(defun compute-edits-for-unuse-package (file unused-package-designator
                                        &optional (package *package*))
  (let* ((unused-package (find-package unused-package-designator))
         (shortest-name (shortest-package-name-loc unused-package))
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
              (destructuring-bind (pos name pkgi pkgmarker)
                  loc
                (list pos :delete (+ (length pkgi)
                                     (length pkgmarker)))))
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
              (destructuring-bind (pos name pkgi pkgmarker)
                  loc
                (list pos :insert prefix)))
            locs)))

(defun compute-edits-for-toggle-qualifier (file package-name)
  (let* ((package (find-package package-name))
         (current-package *package*)
         (on-or-off :unknown)
         (locs
           (locate-symbol-names
            file
            (lambda (name pkgind)
              (when (eq current-package *package*)
                (print (list name on-or-off) *debug-io*)
                (block nil
                  (tagbody again
                     (cond ((and (eq on-or-off :unknown)
                                 (stringp pkgind)
                                 (when (eq package (find-package pkgind))
                                   (setf on-or-off nil) ; set remove qualifiers
                                   (go again))))
                           ((and (eq on-or-off :unknown)
                                 (eq :current pkgind))
                            (let* ((sym (find-symbol name
                                                     current-package))
                                   (pkg (symbol-package sym)))
                              (when (eq pkg package)
                                (setf on-or-off t) ; set add qualifiers
                                (go again))))
                           ((and (null on-or-off)
                                 (stringp pkgind)) ; remove qualifiers
                            (let ((symbol-package
                                    (symbol-package
                                     (find-symbol name pkgind))))
                              (return (eq symbol-package package))))
                           ((and on-or-off
                                 (eq :current pkgind)) ; add qualifiers
                            (let ((symbol-package
                                    (symbol-package
                                     (find-symbol name current-package))))
                              (return (eq symbol-package package))))
                           (t nil)))))))))
    (print locs *debug-io*)
    (if on-or-off
        (let ((qualifier (string-downcase
                          (shortest-package-name-loc package))))
          (mapcar (lambda (x)
                    (destructuring-bind (pos name pkgind &rest rest)
                        x
                      (let ((sym (find-symbol name current-package)))
                        (list pos :insert (concatenate 'string
                                                       qualifier
                                                       (symbol-package-markers-loc
                                                        sym package))))))
                  locs))
        (mapcar (lambda (x)
                  (destructuring-bind (pos name pkgi pkgmarker)
                      x
                    (list pos :delete (+ (length pkgi)
                                         (length pkgmarker)))))
                locs))))

