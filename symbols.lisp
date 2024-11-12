;;;; Requires

(in-package :apprentice) cx

(defun symbol-status (symbol-designator &optional (package *package*))
  (nth-value 1 (find-symbol (string symbol-designator) package)))

(defun symbol-home-package-p (symbol &optional (package *package*))
  (eq package (symbol-package symbol)))

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
