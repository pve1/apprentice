;;;; Requires
;;;;   apprentice
;;;;   alexandria
;;;;   "buttons"
;;;;   "locate-symbols"

;;;; Package apprentice

;; WIP

(in-package :apprentice) cx

(defclass package-apprentice ()
  ()
  (:documentation ""))

(defmethod package-apprentice-toggle-prefix ((ap package-apprentice)
                                             package-designator
                                             buffer)
  (alexandria:when-let ((edits (compute-edits-for-toggle-qualifier
                                (emacs-buffer-string buffer)
                                package-designator)))
    (emacs-perform-edits buffer edits)))

(defmethod package-apprentice-use-package ((ap package-apprentice)
                                           package-designator
                                           &key (package *package*))
  (use-package package-designator package)
  (emacs-message (format nil "Used package ~S" package-designator)))

(defmethod package-apprentice-unuse-package ((ap package-apprentice)
                                             package-designator
                                             &key (package *package*))
  (unuse-package package-designator package)
  (emacs-message (format nil "Unused package ~S" package-designator)))

(defmethod package-apprentice-clear-external ((ap package-apprentice)
                                              package-designator)
  (let ((pkg (find-package package-designator))
        (ext nil))
    (do-external-symbols (sym pkg)
      (push sym ext))
    (unexport ext pkg)))

(defmethod package-apprentice-clear-symbols ((ap package-apprentice)
                                             package-designator)
  (loop :with pkg = (find-package package-designator)
        :for sym :being :each :symbol :in pkg
        :do (unintern sym pkg)))

(defmethod describe-with-apprentice ((ap package-apprentice)
                                     (symbol symbol)
                                     stream)
  (when (and (or (keywordp symbol)
                 (and (symbolp symbol)
                      (null (symbol-package symbol))))
             (find-package symbol))
    (let ((*standard-output* stream)
          (package (find-package symbol)))
      (format t "~A names a package: ~2%" symbol)
      (if (member package (package-use-list *package*))
          (put-lisp-button-here ap
                                "[UNUSE]"
                                `(package-apprentice-unuse-package
                                  *button-apprentice*
                                  ,(package-name package))
                                :redisplay t)
          (put-lisp-button-here ap
                                "[USE]"
                                `(package-apprentice-use-package
                                  *button-apprentice*
                                  ,(package-name package))
                                :redisplay t))
      (princ " ")
      (put-lisp-button-here ap
                            "[QUAL]"
                            `(package-apprentice-toggle-prefix
                              *button-apprentice*
                              ',symbol
                              ,(buffer-context-property :buffer-name)))
      (princ " ")
      (put-lisp-button-here ap
                            "[DEL]"
                            `(delete-package (find-package ',symbol))
                            :redisplay t)
      (princ " ")
      (put-lisp-button-here ap
                            "[CLREXT]"
                            `(package-apprentice-clear-external
                              *button-apprentice* ',symbol)
                            :redisplay t)
      (princ " ")
      (put-lisp-button-here ap
                            "[CLRSYM]"
                            `(package-apprentice-clear-symbols
                              *button-apprentice* ',symbol)
                            :redisplay t)
      (terpri)
      (terpri)
      (describe package)
      (format t "~%Export form:~2%")
      (format t "(EXPORT '(~{~A~^~%          ~}))"
              (sort (loop :for sym :being :each
                          :external-symbol :in package
                          :collect sym)
                    #'string<))
      t)))
