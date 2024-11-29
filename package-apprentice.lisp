;;;; Requires
;;;;   apprentice
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
                                             file)
  (flet ((save ()
           (swank:eval-in-emacs
            `(with-current-buffer (get-file-buffer ,file)
               (when (and (buffer-modified-p)
                          (y-or-n-p (format "Save buffer %s?" ,file)))
                 (save-buffer))))))
    (save)
    (let ((locations (locate-unqualified-symbols-from-package
                      file package-designator)))
      (add-package-prefixes-to-buffer file locations)
      (save))))

(defmethod package-apprentice-use-package ((ap package-apprentice)
                                           package-designator
                                           file
                                           &key (package *package*)
                                                remove-prefixes)
  (use-package (find-package package-designator)
               (find-package package))
  (when (or remove-prefixes
            (swank:eval-in-emacs
             `(with-current-buffer (get-file-buffer ,file)
                (if (y-or-n-p "Remove package prefixes?")
                    (progn
                      (when (and (buffer-modified-p)
                                 (y-or-n-p "Save buffer?"))
                        (save-buffer))
                      ;; Don't continue if buffer is modified.
                      (not (buffer-modified-p)))
                    nil))))
    (emacs-perform-edits
     file
     (compute-edits-for-use-package
      file package-designator package))))

(defmethod package-apprentice-unuse-package ((ap package-apprentice)
                                             package-designator
                                             file
                                             &key (package *package*))
  (when (swank:eval-in-emacs
         `(with-current-buffer (get-file-buffer ,file)
            (if (y-or-n-p "Add package prefixes?")
                (progn
                  (when (and (buffer-modified-p)
                             (y-or-n-p "Save buffer?"))
                    (save-buffer))
                  ;; Don't continue if buffer is modified.
                  (not (buffer-modified-p)))
                nil)))
    (emacs-perform-edits
     file
     (compute-edits-for-unuse-package
      file package-designator package)))
  (unuse-package package-designator package))

(defmethod describe-with-apprentice ((ap package-apprentice)
                                     (symbol symbol)
                                     stream)
  (when (and (or (keywordp symbol)
                 (and (symbolp symbol)
                      (null (symbol-package symbol))))
             (find-package symbol))
    (let ((*standard-output* stream)
          (package (find-package symbol)))
      (format t "~A names a package: " symbol)
      (if (member package (package-use-list *package*))
          (put-lisp-button-here ap
                                "[UNUSE]"
                                `(package-apprentice-unuse-package
                                  *button-apprentice*
                                  ,(package-name package)
                                  ,(buffer-context-property :filename)))
          (put-lisp-button-here ap
                                "[USE]"
                                `(package-apprentice-use-package
                                  *button-apprentice*
                                  ,(package-name package)
                                  ,(buffer-context-property :filename))))
      (princ " ")
      (put-lisp-button-here ap
                            "[DEL]"
                            `(delete-package
                              (find-package ',symbol))
                            :redisplay t)
      (princ " ")
      (put-lisp-button-here ap
                            "[CLREXT]"
                            `(let ((pkg (find-package ',symbol))
                                   (ext '()))
                               (do-external-symbols (sym pkg)
                                 (push sym ext))
                               (unexport ext pkg))
                            :redisplay t)
      (princ " ")
      (put-lisp-button-here ap
                            "[CLRSYM]"
                            `(loop :with pkg = (find-package ',symbol)
                                   :for sym :being
                                   :each :symbol
                                   :in pkg
                                   :do (unintern sym pkg))
                            :redisplay t)
      (princ " ")
      (put-lisp-button-here ap
                            "[PREFIX]"
                            `(package-apprentice-toggle-prefix
                              *button-apprentice*
                              ,symbol
                              ,(buffer-context-property :filename))
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
