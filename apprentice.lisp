;;;; Requires
;;;;   swank
;;;;   eclector
;;;;   "package"

;;; This is Apprentice.  It's purpose is to passively provide helpful
;;; information to the user based on the current state of a Lisp mode
;;; Emacs buffer.  For instance, if the point is on a symbol, then the
;;; apprentice buffer might show the output of cl:describe for that
;;; symbol.

;;; In order to receive more interesting descriptions, the special
;;; variable *apprentice* should be set to something for which a
;;; describe-with-apprentice method is defined.  For example:
;;;
;;; (setf *apprentice* (make-example-apprentice))
;;;
;;; The example apprentice returned by make-example-apprentice happens
;;; to be a pretty good way to get an idea of what Apprentice is all
;;; about, so be sure to try it out.  Calling M-x apprentice-describe
;;; in a Lisp buffer will bring up the Apprentice window.

;;; The generic function describe-with-apprentice is the most
;;; important function in this package.  Its methods should examine
;;; the input object and print something informative to the stream
;;; parameter.

;;; This file contains the Lisp side of the apprentice core.  Whenever
;;; an apprentice window is visible, Emacs calls one of three
;;; functions to receive a description:
;;;
;;; - symbol-description
;;; - presentation-description
;;; - character-description
;;;
;;; A fourth function, form-description, may be called manually by
;;; user to continuously monitor the results of evaluating a form.

;;; In addition, Emacs binds *buffer-context* to a plist with some
;;; suitable properties before calling the functions mentioned above.
;;; The context contains things like the name of the current buffer,
;;; the location of the point, the current package etc.

;;; Special naming conventions used in this package:
;;;
;;; Some functions end in a cryptic two- or three-letter suffix.  They
;;; are considered helper functions local to the file in which they
;;; are defined.  Each file has its own suffix, which allows for more
;;; relaxed naming of helper functions.

(in-package :apprentice) cx ; <-- enables capitalized export

;;; Variables

(defvar *Apprentice* #'describe
  "The currently active apprentice used for generating descriptions.
Should implement DESCRIBE-WITH-APPRENTICE.")

(defvar *Max-description-size* 100000
  "The maximum number of characters that can be returned to Emacs by
RETURN-DESCRIPTION.")

(defvar *Force-return-description* nil
  "If non-nil, return a description even if the current object and its
description are identical to the previous object and description.  Is
bound by Emacs in certain situations.")

(defvar *Buffer-context* nil
  "A plist which may contain properties of the current Emacs buffer.
The Emacs variable APPRENTICE-PROVIDE-CONTEXT determines which
properties are provided.

Examples: :buffer-name :point :region :package :filename

Note: The :point and :region properties contain indexes as returned by
Emacs, i.e. 1-based.")

(makunbound '*buffer-context*)

(defvar *Description-stream* nil
  "Bound to the string stream representing the description currently
being built.  This is the stream parameter of
DESCRIBE-WITH-APPRENTICE.")

(defvar *Before-describe-hook* nil
  "List of functions called with no argument before calling
describe-with-apprentice.")

(defvar *description-properties* nil
  "This variable contains extra information needed to render a
description.  It gets sent to Emacs alongside the description string.
It can be used to place buttons, add faces, indent or font lock
regions.  The function push-description-property is used to add
properties.

See apprentice-insert in apprentice.el.")

(makunbound '*description-properties*)

(defvar *previous-object* nil
  "The previous input object given to DESCRIBE-WITH-APPRENTICE.")

(defvar *previous-description* nil
  "The previous description produced by DESCRIBE-WITH-APPRENTICE.")

;;; Basics

(defgeneric Describe-with-apprentice (apprentice object stream)
  (:documentation "This function should write a description of OBJECT
to STREAM.  The description will be displayed in the Apprentice Emacs
window.  The generation of the description should be efficient enough
to allow this function to be called roughly every time the point
becomes idle, or periodically, according to the emacs variable
APPRENTICE-POLLING-FREQUENCY.")
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

;; May get removed
(defgeneric describe-tagged-list (apprentice tag plist stream)
  (:method (apprentice tag plist stream)
    nil))

;; May return a string, a list of two elements (a string and a list
;; of text properties), :unchanged or :max-size-exceeded.
(defun return-description (object desc)
  "Checks if the description DESC is the same as the last description,
and that it isn't too big."
  (check-type desc (or string cons))
  (if (<= (length desc) *max-description-size*)
      (prog1 (if (and (equal *previous-object* object)
                      (equal *previous-description* desc)
                      (not *force-return-description*))
                 :unchanged
                 (if (and (boundp '*description-properties*)
                          (not (null *description-properties*)))
                     (list desc *description-properties*)
                     desc))
        (setf *previous-object* object
              *previous-description* desc))
      :max-size-exceeded))

(defun Push-description-property (property &optional where)
  "Adds PROPERTY to the the list of description properties about to be
sent over to Emacs.  PROPERTY should be a list whose car is a symbol
indicating the type of property.

Example: (lisp-button 10 17 \"[HELLO]\" ...)"
  (check-type where (member nil :last))
  (if (eq where :last)
      (let ((last (last *description-properties*)))
        (rplacd last (cons property nil)))
      (push property *description-properties*)))

(defun Buffer-context-property (property)
  "Queries the current buffer context for PROPERTY."
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

;;;; Describing presentations.

(defun Presentation-description (presentation-id)
  "Returns the a description of the Slime presentation object with id
PRESENTATION-ID.  Only works if the function
SWANK:LOOKUP-PRESENTED-OBJECT is defined. "
  (let* ((*package* (suggested-current-package))
         (*description-properties* nil)
         (desc (with-output-to-string (s)
                 (run-hook *before-describe-hook*)
                 (describe-with-apprentice
                  *apprentice*
                  (funcall-maybe '#:swank '#:lookup-presented-object
                                 presentation-id)
                  s))))
    (when desc
      (return-description presentation-id desc))))

;;;; Resolving symbols

;;; Eclector is used to read symbols in order to easily avoid
;;; unnecessary interning as well as get information on package
;;; qualifiers that would otherwise be tricky.

(defmacro with-eclector-client (client &body body)
  `(let ((*read-eval* nil)
         (eclector.base:*client* ,client))
     ,@body))

;;; Used to detect a lone package qualifier, e.g. "cl-user:". Will
;;; throw the empty symbol.  Apropos-apprentice makes use of this to
;;; list all (external) symbols in a package.
(defmethod eclector.reader:check-symbol-token
    ((client (eql 'default-resolve-symbol))
     input-stream
     token
     escape-ranges
     position-package-marker-1
     position-package-marker-2)
  (handler-case (call-next-method)
    (eclector.reader:symbol-name-must-not-end-with-package-marker ()
      (let ((symbol (make-symbol ""))
            (package-indicator
              (if position-package-marker-2
                  (subseq token 0 (- (length token) 2))
                  (subseq token 0 (1- (length token))))))
        (setf (get symbol 'package-indicator) package-indicator
              (get symbol 'unspecified-symbol) t
              ;; We add this the buffer context mainly for apropos-apprentice.
              ;; This is also done in interpret-symbol below.
              (getf *buffer-context* 'package-indicator) package-indicator)
        (throw 'symbol symbol)))))

;; Handle cl:nil specially.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+nil+)
    (defconstant +nil+ (make-symbol "NIL"))))

;; This method returns either symbols that already exist, or
;; uninterned symbols.  It will not intern new symbols.
(defmethod eclector.reader:interpret-symbol
    ((client (eql 'default-resolve-symbol))
     input-stream
     package-indicator
     symbol-name
     internp)
  (flet ((anonymous-symbol (pkg)
           (let ((sym (make-symbol symbol-name)))
             (setf (get sym 'package-indicator) pkg)
             sym)))
    ;; We add this the buffer context mainly for apropos-apprentice.
    (setf (getf *buffer-context* 'package-indicator)
          package-indicator)
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

(defun suggested-current-package ()
  "Tries to return the \"current\" package.  Uses the :package buffer
context property, which is the result of calling
slime-current-package.  If that fails, returns *package*."
  (alexandria:when-let ((pkg (buffer-context-property
                              'suggested-current-package)))
    (return-from suggested-current-package pkg))
  (let* ((pkg-string (buffer-context-property :package))
         (*read-eval* nil)
         (pkg (when pkg-string
                (find-package (read-from-string pkg-string)))))
    (setf (getf *buffer-context* 'suggested-current-package)
          pkg)
    (or pkg *package*)))

(defun suggested-current-package-name ()
  "Returns the name of the suggested current package."
  (package-name (suggested-current-package)))

(defgeneric Resolve-symbol (apprentice symbol-name)
  (:documentation "This function should resolve the string SYMBOL-NAME
into a symbol."))

(defmethod resolve-symbol (apprentice symbol-name)
  (let* ((*package* (suggested-current-package))
         (*read-eval* nil))
    (with-eclector-client 'default-resolve-symbol
      (catch 'symbol
        (eclector.reader:read-from-string symbol-name)))))

;;;; Describing symbols.

(defun Symbol-description (symbol-name)
  "Returns a description of the symbol received by resolving the string
SYMBOL-NAME."
  (check-type symbol-name string)
  (let* ((*package* (suggested-current-package))
         (symbol (ignore-errors
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

;;;; Describing forms

(defgeneric Read-from-string-with-apprentice (apprentice string)
  (:documentation "Reads from STRING in the context of APPRENTICE.")
  (:method (apprentice string)
    (read-from-string string)))

(defgeneric Eval-with-apprentice (apprentice form)
  (:documentation "Evaluates FORM in the context of APPRENTICE.")
  (:method (apprentice form)
    (eval form)))

(defun describe-form-separator (label &optional (width 60) newline)
  "Generates the separator printed by DESCRIBE-FORM-WITH-APPRENTICE."
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

;;; Reads the package designator string returned by
;;; (slime-current-package).

(defgeneric Read-package-designator-string (apprentice
                                            package-designator-string)
  (:documentation
   "Reads a string containing a package designator, e.g. \":foo\".")
  (:method (apprentice package-designator-string)
    (let ((*read-eval* nil))
      (read-from-string-with-apprentice apprentice
                                        package-designator-string))))

;;; Package-designator is the string returned by
;;; (slime-current-package). It must be READ to get the real
;;; package-designator.

(defgeneric Describe-form-with-apprentice (apprentice
                                           form-string
                                           package-designator-string)
  (:documentation
   "Reads FORM-STRING and evaluates the result. Output to
*STANDARD-OUTPUT* and *ERROR-OUTPUT* is captured and a description
string combining the output and result is returned.  This is done with
*PACKAGE* bound to the package indicated by
PACKAGE-DESIGNATOR-STRING.")
  (:method (apprentice (form-string string) package-designator)
    (with-output-to-string (*standard-output*)
      (let ((*error-output* *standard-output*)
            (*description-stream* *standard-output*)
            (*description-properties* nil))
        (handler-case
            (let* ((real-package-designator
                     (read-package-designator-string
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
  "Returns a description of the form contained in FORM-STRING using
DESCRIBE-FORM-WITH-APPRENTICE."
  (check-type form-string string)
  (assert (null (boundp '*description-properties*)))
  (return-description
   (list form-string package-designator)
   (describe-form-with-apprentice *apprentice*
                                  form-string
                                  package-designator)))

;;; Describing "characters", i.e. when (thing-at-point 'symbol)
;;; returns nil.

(defclass Looking-at-character ()
  ((preceding-char :initarg :preceding-char
                   :accessor Preceding-char
                   :initform nil)
   (following-char :initarg :following-char
                   :accessor Following-char
                   :initform nil))
  (:documentation "This class is used as input to
DESCRIBE-WITH-APPRENTICE in situations where the point is not on
anything that Emacs recognizes as a symbol (or number)
whitespace (using thing-at-point)."))

(defmethod print-object ((l looking-at-character) stream)
  (print-unreadable-object (l stream :type t)
    (format stream "~S ~S"
            (preceding-char l)
            (following-char l)))
  l)

(defun Character-description (preceding-char following-char)
  "Returns a description relating to the current position of the point
when SYMBOL-DESCRIPTION is not applicable."
  (let* ((*package* (suggested-current-package))
         (*description-properties* nil))
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

;;; Macros for adding faces to text.
;;;
;;; Use M-x list-faces-display to see available faces.
;;;
;;; Examples:
;;; (with-face ("font-lock-keyword-face")
;;;   (princ "Hello"))
;;; (with-face ('(:foreground "Royal Blue"))
;;;   (princ "World!"))

(defmacro With-face ((face
                      &optional
                      (stream '*standard-output*)
                      (offset 0))
                     &body body)
  "Adds the face FACE to anything printed to the stream named by the
symbol STREAM.  Use OFFSET to handle nested WITH-OUTPUT-TO-STRING
forms."
  (alexandria:with-gensyms (begin)
    `(let ((,begin (+ ,offset (file-position ,stream))))
       (prog1 (progn ,@body)
         (push-description-property
          (list 'add-face
                ,begin
                (+ ,offset (file-position ,stream))
                ,face))))))

;;; Use M-x list-colors-display to see available colors.
;;;
;;; (defmethod describe-with-apprentice ((apprentice my-apprentice)
;;;                                      object
;;;                                      stream)
;;;   (let ((*standard-output* stream))
;;;     (with-foreground-color ("Dark Orchid")
;;;       (princ "abc"))
;;;     (with-foreground-color ("#8810ff")
;;;       (princ "def"))
;;;     (with-foreground-color ("#ffff1c710000")
;;;       (princ "ghi"))))

(defmacro With-foreground-color ((color
                                  &optional
                                  (stream '*standard-output*)
                                  (offset 0))
                                 &body body)
  "Sets the foreground color of anything printed to the stream named by
the symbol STREAM within BODY to COLOR.  Use OFFSET to handle nested
WITH-OUTPUT-TO-STRING forms."
  `(with-face ('(:foreground ,color) ,stream ,offset)
     ,@body))
