;;;; Requires
;;;;   apprentice
;;;;   cl-interpol
;;;;   cl-ppcre
;;;;   "buttons"
;;;;   "emacs"
;;;;   "option"
;;;;   "system"
;;;;   "exec-apprentice"

(cl-interpol:enable-interpol-syntax)

(in-package :apprentice) cx

;;; Making basic cores

(defclass core-apprentice (exec-apprentice)
  ()
  (:default-initargs
   :modes (option "Mode" `((basic-core exec-basic-core)))
   :arg-parsing-methods (option "Arg parsing" nil))
  (:documentation ""))

(defmethod describe-with-apprentice ((ap core-apprentice)
                                     (object looking-at-character)
                                     stream)
  (alexandria:when-let* ((file (buffer-context-property :filename))
                         (system (source-file-system-name file)))
    (executable-apprentice-describe
     ap object stream
     :entry-point (string-downcase
                   (package-name
                    (suggested-current-package)))
     :system system
     :file file)))
