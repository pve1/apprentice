;;;; Requires 
;;;;   "package"

(in-package :slime-apprentice) [*]

(unless (find-package "SLIME-APPRENTICE-READ")
  (make-package "SLIME-APPRENTICE-READ" :use nil))

(defvar *apprentice* nil)
(defvar *previous-description* nil)
(defvar *previous-object* nil)
(defvar *max-description-size* 10000)
(defvar *force-return-description* nil)

(defgeneric Description (apprentice object stream)
  (:method (apprentice object stream)
    (describe object stream)))

(defun return-description (object desc)
  (if (<= (length desc) *max-description-size*)
      (prog1 (if (and (eql *previous-object* object)
                      (equal *previous-description* desc)
                      (not *force-return-description*))
                 :unchanged
                 desc)
        (setf *previous-object* object
              *previous-description* desc))
      :max-size-exceeded))

(defun Presentation-description (presentation-id)
  (let* ((desc (with-output-to-string (s)
                (description *apprentice*
                             (swank:lookup-presented-object
                              presentation-id)
                             s))))
    (return-description presentation-id desc)))

;; Read symbol-name in an empty, special package and try to figure out
;; which symbol was actually meant.
(defun Symbol-description (symbol-name)
  (check-type symbol-name string)
  (let* ((preliminary-symbol
           (ignore-errors
            (let ((*package* (find-package "SLIME-APPRENTICE-READ"))
                  (*read-eval* nil))
              (read-from-string symbol-name))))
         (actual-symbol
           ;; Either foo:bar, :foo or  #:foo was read.
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
        (let ((desc (with-output-to-string (s)
                      (description *apprentice* actual-symbol s))))
          (return-description actual-symbol desc))
        nil)))

