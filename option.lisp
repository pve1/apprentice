;;;; Requires alexandria

;;;; Option

(in-package :apprentice)

;;; An option consists of several alternatives, one of which can be
;;; currently selected (think radio buttons). An option may be empty,
;;; in which case nothing is selected.

;;; An alternative is specified as a list of the form
;;; (LABEL VALUE . PROPERTIES).

;;; The properties can be symbols (flags) or a two-element list of the
;;; form (PROPERTY VALUE).

;;; Example: (foo 123 (color red) primary)

;;; TODO: Maybe generalize into a conceptually simpler "tree of
;;; options" instead of the suboptions hack.

;;; Note: This class was implemented because various "dumb" approaches
;;; to solve this problem weren't good enough.

(defclass option ()
  ((name :initarg :name
         :accessor name
         :initform nil)
   (alternatives :initarg :alternatives
                 :accessor alternatives
                 :initform nil)
   ;; One of the alternatives
   (selected :initarg :selected
             :accessor selected
             :initform nil)
   (select-hook :initarg :select-hook
                :accessor select-hook
                :initform nil)
   (select-value-function :initarg :select-value-function
                          :accessor select-value-function
                          :initform nil))
  (:documentation ""))

(defstruct (option-alternative
            (:type vector)
            (:constructor option-alternative
                (label value properties)))
  (head 'alternative) label value properties)

(defun consify-attributes-opt (attribute-list)
  (flet ((listify-property (x)
           (if (atom x)
               (cons x x)
               (cons (first x)
                     (second x)))))
    (mapcar #'listify-property attribute-list)))

(defun alist-update-opt (akey datum alist &key (test #'eql)
                                               (key #'identity))
  (let ((prop (assoc akey alist :test test :key key)))
    (if prop
        (progn
          (setf (cdr prop) datum)
          alist)
        (acons akey datum alist))))

(defmethod initialize-instance :after ((o option) &key unselected
                                                       value-init)
  (flet ((value-init-maybe (x)
           (if value-init
               (funcall value-init x)
               x)))
    ;; Note: alternatives may be empty, that's ok.
    (setf (alternatives o)
          (mapcar
           (lambda (x)
             (if (consp x)
                 (destructuring-bind (label value &rest properties)
                     x
                   (assert (atom label))
                   (option-alternative
                    label
                    (value-init-maybe value)
                    (consify-attributes-opt properties)))
                 (option-alternative
                  x (value-init-maybe x) nil)))
           (alternatives o)))
    (unless unselected
      (setf (selected o) (first (alternatives o))))))

(defun option (name alternatives &key value-init
                                      select-hook
                                      select-value-function
                                      unselected)
  (make-instance 'option
    :name name
    :alternatives alternatives
    :select-hook select-hook
    :select-value-function select-value-function
    :unselected unselected
    :value-init value-init))

(defmethod print-object ((o option) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~A" (name o)))
  o)

(defmethod option-find-alternative (option label)
  (let ((alt (find label (alternatives option)
                   :test #'equal
                   :key #'option-alternative-label)))
    (unless alt
      (error "Unknown alternative: ~S." label))
    alt))

(defmethod option-select (option label)
  (prog1 (setf (selected option) (option-find-alternative option
                                                          label))
    (alexandria:when-let ((hooks (alexandria:ensure-list
                                  (select-hook option))))
      (dolist (hook hooks)
        (funcall hook option label)))))

(defmethod option-unselect (option)
  (setf (selected option) nil))

(defmethod option-true-value (option alternative)
  (if (select-value-function option)
      (funcall (select-value-function option)
               (option-alternative-value alternative))
      (option-alternative-value alternative)))

(defmethod option-alternative-property (alternative property)
  (alexandria:assoc-value
   (option-alternative-properties alternative)
   property))

(defmethod (setf option-alternative-property) (new
                                               alternative
                                               property)
  (setf (option-alternative-properties alternative)
        (alist-update-opt property
                          new
                          (option-alternative-properties
                           alternative)))
  new)

(defmethod option-alternative-labels (option)
  (mapcar #'option-selected-label (alternatives option)))

;;; On currently selected alternative.

(defmethod option-selected-label (option)
  (option-alternative-label (selected option)))

(defmethod option-selected-property (option property)
  (option-alternative-property
   (selected option)
   property))

(defmethod option-selected-value (option)
  (when (selected option)
    (option-true-value option (selected option))))

;; Move this elsewhere?
(defmethod option-selected-message (option)
  (let ((alt (selected option)))
    (or (option-alternative-property alt 'message)
        (option-alternative-label alt))))

;;; Suboptions

;; To allow for empty options.
(defmethod option-get-suboptions ((alternative null))
  nil)

(defmethod option-get-suboptions (alternative)
  (option-alternative-property alternative 'suboptions))

(defmethod option-get-suboptions ((option option))
  (when (selected option)
    (option-get-suboptions (selected option))))

;; Find the deepest suboption.
(defmethod option-deepest-suboption ((option option))
  (alexandria:when-let ((sub (option-get-suboptions
                              (selected option))))
    (or (option-deepest-suboption sub)
        sub)))

;; Return the path of selected options, deepest first.
(defmethod option-suboption-path ((option option))
  (labels ((walk (opt acc)
             (alexandria:if-let
                 ((sub (option-get-suboptions
                        (selected opt))))
               (walk sub (cons sub acc))
               acc)))
    (walk option (list option))))

(defmethod option-selected-suboption ((option option))
  (first (option-suboption-path option)))

(defmethod option-selected-suboption-value ((option option))
  (option-selected-value
   (option-selected-suboption option)))
