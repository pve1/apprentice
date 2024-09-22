;;;; Requires
;;;;   "slime-apprentice"

(in-package :slime-apprentice) cx

(defmethod put-button-here (apprentice button-type label when-clicked
                            &key (stream *standard-output*)
                                 (offset 0)
                                 face)
  (let ((here (file-position stream))
        (there)
        (offset* (typecase offset
                   (stream (or (file-position offset)
                               0))
                   (integer offset))))
    (unless here
      (warn "No file position available for stream ~S." stream))
    (princ label stream)
    (setf there (file-position stream))
    (push (list button-type
                (+ here offset*)
                (+ there offset*)
                label
                when-clicked
                face)
          *description-properties*)))

(defmethod put-lisp-button-here (apprentice label when-clicked
                                 &key (stream *standard-output*)
                                      (offset 0)
                                      face)
  (put-button-here apprentice 'lisp-button label when-clicked
                   :face face
                   :offset offset
                   :stream stream))

;; Fixme: Depends on internal swank function.
(defmethod put-elisp-button-here (apprentice label when-clicked
                                  &key (stream *standard-output*)
                                       (offset 0)
                                       face)
  (put-button-here apprentice
                   'elisp-button
                   label
                   (swank::process-form-for-emacs when-clicked)
                   :face face
                   :offset offset
                   :stream stream))
