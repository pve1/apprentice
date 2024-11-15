;;;; Requires
;;;;   "apprentice"

(in-package :apprentice) cx

(defvar *button-callbacks* (make-hash-table))
(defvar *button-callback-id-counter* 0)
(defvar *button-apprentice*)

(defun initialize-button-callbacks ()
  (setf *button-callback-id-counter* 0)
  (clrhash *button-callbacks*))

(pushnew 'initialize-button-callbacks *before-describe-hook*)

(defmethod make-button-callback (ap callback)
  (let ((id (incf *button-callback-id-counter*)))
    (setf (gethash *button-callback-id-counter*
                   *button-callbacks*)
          (list ap callback))
    id))

(defmethod make-button-callback-form (ap when-clicked)
  `(eval-button-callback
    ,(make-button-callback ap when-clicked)))

(defun lookup-button-callback (id)
  (lookup-button-callback-with-apprentice *apprentice* id))

(defun eval-button-callback (id)
  (eval-button-callback-1 *apprentice* id))

(defmethod lookup-button-callback-with-apprentice (apprentice id)
  (gethash id *button-callbacks*))

;; apprentice is *apprentice*
(defmethod eval-button-callback-1 (apprentice id)
  (destructuring-bind (ap callback)
      (lookup-button-callback-with-apprentice apprentice id)
    (eval-button-callback-2 ap callback)))

;; apprentice is the one that created the button
(defmethod eval-button-callback-2 (apprentice (callback list))
  (let ((*button-apprentice* apprentice))
    (eval callback)))

(defmethod eval-button-callback-2 (apprentice (callback symbol))
  (let ((*button-apprentice* apprentice))
    (funcall callback apprentice)))

(defmethod eval-button-callback-2 (apprentice (callback function))
  (let ((*button-apprentice* apprentice))
    (funcall callback apprentice)))

(defmethod make-button (apprentice button-type label when-clicked
                        &key begin end
                             face
                             redisplay
                             name arguments
                             skippable
                             &allow-other-keys)
  (list button-type
        begin
        end
        label
        when-clicked
        :face face
        :redisplay redisplay
        :name name
        :arguments arguments
        :skippable skippable))

(defmethod put-button-here (apprentice button-type label when-clicked
                            &key (stream *standard-output*)
                                 (offset 0)
                                 face
                                 redisplay
                                 name      ; only for elisp
                                 arguments ; only for elisp
                                 skippable)
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
    (push-description-property
     (make-button apprentice
                  button-type
                  label
                  when-clicked
                  :begin (+ here offset*)
                  :end (+ there offset*)
                  :face face
                  :redisplay redisplay
                  :name name
                  :arguments arguments
                  :skippable skippable))))

(defmethod Put-lisp-button-here (apprentice label when-clicked
                                 &key (stream *standard-output*)
                                      (offset 0)
                                      face
                                      redisplay
                                      name
                                      arguments
                                      skippable)
  (unless when-clicked
    (setf when-clicked
          `(button-pressed *button-apprentice* ,label)))
  (put-button-here apprentice
                   'lisp-button
                   label
                   (make-button-callback-form
                    apprentice when-clicked)
                   :face face
                   :offset offset
                   :stream stream
                   :redisplay redisplay
                   :name name
                   :arguments arguments
                   :skippable skippable))

;; Fixme: Depends on internal swank function.
(defmethod Put-elisp-button-here (apprentice label when-clicked
                                  &key (stream *standard-output*)
                                       (offset 0)
                                       face
                                       redisplay
                                       name
                                       arguments
                                       skippable)
  (put-button-here apprentice
                   'elisp-button
                   label
                   (swank::process-form-for-emacs when-clicked)
                   :face face
                   :offset offset
                   :stream stream
                   :redisplay redisplay
                   :name name
                   :arguments arguments
                   :skippable skippable))

(defmethod Create-ephemeral-elisp-function (apprentice symbol lambda-form)
  (push-description-property
   (list 'ephemeral-function
              :name symbol
              :lambda-string (swank::process-form-for-emacs
                              lambda-form))))

(defun Emacs-message (object)
  (swank:eval-in-emacs `(message ,(princ-to-string object))))

(defgeneric Buttons (apprentice)
  (:documentation "Returns the buttons of an apprentice."))

(defgeneric Find-button (apprentice button-name)
  (:method (apprentice button-name)
    (let ((buttons (buttons apprentice)))
      (typecase buttons
        (list (cdr (assoc button-name buttons
                          :test #'equal)))
        (hash-table (gethash button-name buttons)))))
  (:documentation ""))

(defgeneric Button-pressed (apprentice button)
  (:method (apprentice (button-name string))
    (alexandria:if-let ((button (find-button apprentice
                                             button-name)))
      (button-pressed apprentice button)
      (error "No button named ~S." button-name)))
  (:method (apprentice (button function))
    (funcall button apprentice))
  (:documentation ""))
