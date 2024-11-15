;;;; Requires
;;;;   apprentice

(in-package :apprentice) cx

(defclass Temporary-apprentice ()
  ((original-apprentice :initarg :original-apprentice
                        :accessor Original-apprentice
                        :initform *apprentice*)
   (describe-function :initarg :describe-function
                      :accessor Describe-function
                      :initform nil)
   (done :initarg :done
         :accessor Done
         :initform t)))

(defmethod describe-with-apprentice ((ap temporary-apprentice)
                                     object stream)
  (funcall (describe-function ap) ap object stream))

(defmethod describe-with-apprentice :around ((ap temporary-apprentice)
                                             object 
                                             stream)
  (unwind-protect (call-next-method)
    (when (done ap)
      (setf *apprentice* (original-apprentice ap)))))

(defun Set-temporary-apprentice (apprentice &optional describe-function)
  (when (and apprentice (symbolp apprentice))
    (setf apprentice (make-instance apprentice)))
  (setf *apprentice*
        (make-instance 'temporary-apprentice
          :describe-function 
          (if describe-function
              (lambda (ap obj str)
                (declare (ignore ap))
                (funcall describe-function apprentice obj str))
              (lambda (ap object stream)
                (declare (ignore ap))
                (describe-with-apprentice apprentice object stream)))))
  t)
