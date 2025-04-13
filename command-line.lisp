;;;; Requires alexandria

;;; Simple command line argument parsing.
;;;
;;; Todo: Handle end-of-arguments argument, like "--" in many gnu
;;; utils.
;;;
;;; Provides
;;;
;;; (parse-standard-command-line-arguments '("--count" "1"
;;;                                          "-ab"
;;;                                          "foo"
;;;                                          "bar"))
;;; => ("foo" "bar" :COUNT 1 :A T :B T)
;;;
;;; (parse-natural-style-command-line-arguments '("xyz:" "yes"
;;;                                              "count:" "1"
;;;                                              "foo"
;;;                                              "bar"))
;;; => ("foo" "bar" :XYZ "yes" :COUNT 1)
;;;
;;; (parse-lisp-style-command-line-arguments '("foo"
;;;                                            "bar"
;;;                                            ":xyz" "yes"
;;;                                            ":count" "1"))
;;; => ("foo" "bar" :XYZ "yes" :COUNT 1)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:apprentice)
    (make-package '#:apprentice :use '(#:cl))))

(in-package :apprentice)

(defun keywordify-cli (string-designator &optional preserve-case)
  (intern (if preserve-case
              (string string-designator)
              (string-upcase (string string-designator)))
          (find-package :keyword)))

(defun headp-cli (head-symbol list)
  (and (consp list)
       (eq (car list) head-symbol)))

;;; Flag readers, -f, --foo, -xyz, :foo, foo:

(defun read-lisp-style-option-cli (arg)
  (when (alexandria:starts-with #\: arg)
    (assert (< 1 (length arg)))
    (keywordify-cli (subseq arg 1))))

(defun read-natural-style-option-cli (arg)
  (when (alexandria:ends-with #\: arg)
    (assert (< 1 (length arg)))
    (keywordify-cli (subseq arg 0 (1- (length arg))))))

;;; Note: A long option ("--foo") requires a value argument, whereas a
;;; short option ("-x") is a boolean flag of one character with no
;;; value argument. Several boolean flags can be specified together,
;;; i.e. "-xyz" -> (:x t :y t :z t).
(defun read-standard-option-cli (arg)
  (cond ((alexandria:starts-with-subseq "--" arg)
         (assert (< 2 (length arg)))
         (keywordify-cli (subseq arg 2)))
        ;; Boolean flags
        ((alexandria:starts-with-subseq "-" arg)
         (assert (< 1 (length arg)))
         (cons 'flags (map 'list
                           #'keywordify-cli
                           (subseq arg 1))))
        (t nil)))

;;; Argument and option readers

;;; Normal argument

(defun argument-reader-cli (args)
  (values (list 'argument (first args))
          (rest args)))

;;; Option with value
;;; Transforms sets of flags ("-xyz", (:x :y :z)) into (flags :x t :y t :z t)
;;; Transforms options "--foo bar" into (option :foo "bar")
;;; Returns nil if no option
(defun make-option-reader-cli (&key (flag-reader 'read-standard-option-cli)
                                    (parse-integers t))
  (lambda (args)
    (when args
      (let* ((flag (funcall flag-reader (car args)))
             (tail (cdr args)))
        (cond ((headp-cli 'flags flag)
               ;; List of boolean flags
               (values (cons 'flags (loop :for f :in (cdr flag)
                                          :append (list f t)))
                       tail))
              (flag
               (when (null tail)
                 (error "No value provided for option ~A." flag))
               (values
                (cons 'option
                      (list flag
                            (if parse-integers
                                (handler-case (parse-integer (car tail))
                                  (error () (car tail)))
                                (car tail))))
                (cdr tail)))
              (t nil))))))

;;; Try all the readers to get a single non-nil value. Also return the
;;; remaining args.
(defun read-next-argument-cli (readers args)
  (when args
    (dolist (reader readers)
      (multiple-value-bind (result remaining)
          (funcall reader args)
        (when result
          (return-from read-next-argument-cli
            (values result remaining)))))))

;;; Process all args using the given readers.
(defun apply-readers-to-arguments-cli (readers args)
  (loop :with tail = args
        :until (endp tail)
        :collect (multiple-value-bind (result remaining)
                     (read-next-argument-cli readers tail)
                   (when result
                     (assert (not (eq tail remaining))))
                   (setf tail remaining)
                   result)))

;;; Transform the result of apply-readers-to-arguments-cli into a
;;; plist.
(defun processed-arguments-to-plist-cli (processed-args)
  (let (options arguments)
    (dolist (arg processed-args)
      (case (car arg)
        ((or flags option)
         (dolist (x (cdr arg))
           (push x options)))
        (argument (push (second arg) arguments))))
    (list :options (nreverse options)
          :arguments (nreverse arguments))))

(defun parse-command-line-arguments-using-spec (spec args)
  (let* ((option-reader (apply #'make-option-reader-cli spec))
         (readers (list option-reader
                        'argument-reader-cli)))
    (destructuring-bind (&key options arguments)
        (processed-arguments-to-plist-cli
         (apply-readers-to-arguments-cli readers args))
      (append arguments options))))

(defun parse-lisp-style-command-line-arguments (args)
  "$ myprogram :foo 1 :bar 2 abc xyz
$ myprogram abc xyz :foo 1 :bar 2"
  (let* ((spec '(:flag-reader read-lisp-style-option-cli
                 :parse-integers t)))
    (parse-command-line-arguments-using-spec spec args)))

(defun parse-natural-style-command-line-arguments (args)
"$ myprogram foo: 1 bar: 2 abc xyz
$ myprogram abc xyz foo: 1 bar: 2"
  (let* ((spec '(:flag-reader read-natural-style-option-cli
                 :parse-integers t)))
    (parse-command-line-arguments-using-spec spec args)))

(defun parse-standard-command-line-arguments (args)
  "$ myprogram --foo 1 --bar 2 abc xyz
$ myprogram abc xyz --foo 1 --bar 2
$ myprogram -qwe abc xyz             (boolean flags)"
  (let* ((spec '(:flag-reader read-standard-option-cli
                 :parse-integers t)))
    (parse-command-line-arguments-using-spec spec args)))
