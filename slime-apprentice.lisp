;;;; Requires
;;;;   swank
;;;;   "package"

(in-package :slime-apprentice) [*]

(unless (find-package "SLIME-APPRENTICE-READ")
  (make-package "SLIME-APPRENTICE-READ" :use nil))

(defvar *Apprentice* nil)
(defvar *Max-description-size* 10000)
(defvar *Force-return-description* nil)
(defvar *previous-object* nil)
(defvar *previous-description* nil)

(defgeneric Describe-with-apprentice (apprentice object stream)
  (:method (apprentice object stream)
    (describe object stream)))

(defgeneric Describe-unknown-symbol (apprentice symbol-name stream)
  (:method (apprentice (symbol-name string) stream) ; read error, e.g "cl:mapca"
    nil)
  (:method (apprentice (symbol-name symbol) stream) ; uninterned symbol
    nil))

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

(defun Presentation-description (presentation-id)
  (let* ((desc (with-output-to-string (s)
                 (describe-with-apprentice *apprentice*
                                           (swank:lookup-presented-object
                                            presentation-id)
                                           s))))
    (when desc
      (return-description presentation-id desc))))

;; Read symbol-name in an empty, special package and try to figure out
;; which symbol was actually meant.
(defun Symbol-description (symbol-name)
  (check-type symbol-name string)
  (let* ((preliminary-symbol
           (ignore-errors
            (let* ((*package* (find-package "SLIME-APPRENTICE-READ"))
                   (*read-eval* nil)
                   (result (read-from-string symbol-name)))
              (when (and result (symbolp result))
                result))))
         (actual-symbol
           ;; Either foo:bar, :foo or  #:foo was read.
           ;; TODO: Check for a keyword before reading, so we don't
           ;; intern unnecessarily.
           (cond ((and preliminary-symbol
                       (symbolp preliminary-symbol)
                       (not (eql (symbol-package preliminary-symbol)
                                 (find-package "SLIME-APPRENTICE-READ"))))
                  preliminary-symbol)
                 ;; Otherwise, if a symbol with the name SYMBOL-NAME exists
                 ;; in *package*, take that.
                 ((and preliminary-symbol
                       (symbolp preliminary-symbol)
                       (find-symbol (symbol-name preliminary-symbol)))))))
    (when (symbolp preliminary-symbol)
      (unintern preliminary-symbol (find-package "SLIME-APPRENTICE-READ")))
    (if actual-symbol
        (return-description
         actual-symbol
         (with-output-to-string (s)
           (describe-with-apprentice *apprentice*
                                     actual-symbol
                                     s)))
        (if (and preliminary-symbol ; #:foo
                 (symbolp preliminary-symbol))
            (return-description
             preliminary-symbol
             (with-output-to-string (s)
               (describe-unknown-symbol *apprentice*
                                        preliminary-symbol
                                        s)))

            (return-description
             symbol-name
             (with-output-to-string (s)
               (describe-unknown-symbol *apprentice*
                                        symbol-name
                                        s)))))))
