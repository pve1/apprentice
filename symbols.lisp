;;;; Requires
;;;;   apprentice/package

(in-package :apprentice) cx

;; These functions work differently depending on if the
;; symbol-designator is a string or a symbol. For strings, it's
;; straightforward, but SYMBOL-STATUS additionally checks that
;; whatever FIND-SYMBOL returns is EQ to the argument symbol.

(defun symbol-status (symbol-designator &optional (package *package*))
  (typecase symbol-designator
    ;; Also keyword
    (symbol (multiple-value-bind (symbol status)
                (find-symbol (string symbol-designator)
                             package)
              (if (eq symbol symbol-designator)
                  status
                  nil)))
    (string (nth-value 1 (find-symbol
                          (string symbol-designator)
                          package)))))

(defun symbol-home-package-p (symbol &optional (package *package*))
  (eq package (symbol-package symbol)))

(defun symbol-accessible-p (symbol-designator &optional (package *package*))
  (not (null (symbol-status symbol-designator package))))

(defun symbol-present-p (symbol-designator &optional (package *package*))
  (let ((status (symbol-status symbol-designator package)))
    (or (eq status :internal)
        (eq status :external))))

(defun symbol-internal-p (symbol-designator &optional (package *package*))
  (let ((status (symbol-status symbol-designator package)))
    (eq status :internal)))

(defun symbol-external-p (symbol-designator &optional (package *package*))
  (let ((status (symbol-status symbol-designator package)))
    (eq status :external)))

(defun symbol-inherited-p (symbol-designator &optional (package *package*))
  (let ((status (symbol-status symbol-designator package)))
    (eq status :inherited)))

(defun symbol-uninterned-p (symbol)
  (null (symbol-package symbol)))
