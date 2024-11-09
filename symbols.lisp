;;;; Requires

(in-package :apprentice) cx

(defun symbol-status (symbol &optional (package *package*))
  (nth-value 1 (find-symbol (string symbol) package)))

(defun symbol-home-package-p (symbol &optional (package *package*))
  (eq package (symbol-package symbol)))

(defun symbol-present-p (symbol &optional (package *package*))
  (let ((status (symbol-status symbol package)))
    (or (eq status :internal)
        (eq status :external))))

(defun symbol-internal-p (symbol &optional (package *package*))
  (let ((status (symbol-status symbol package)))
    (eq status :internal)))

(defun symbol-external-p (symbol &optional (package *package*))
  (let ((status (symbol-status symbol package)))
    (eq status :external)))

(defun symbol-inherited-p (symbol &optional (package *package*))
  (let ((status (symbol-status symbol package)))
    (eq status :inherited)))

