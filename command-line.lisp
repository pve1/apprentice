;;; === Overly flexible command line option parsing ===
;;;
;;; Aims to make it convenient to call lisp functions with arguments
;;; received from the command line.
;;;
;;; === Usage ===
;;;
;;; To use it, call parse-command-line-options-flexible on the command
;;; line arguments, like this:
;;;
;;; (apply #'my-function
;;;        (parse-command-line-options-flexible
;;;         '("--verbose"
;;;           "--foo:1" "--bar:" "2" "--abc=3" ":qwe" "4"
;;;           "-a:1" "-b=2"
;;;           "-xyz"
;;;           "hello" "world")))
;;; ~~>
;;; (apply #'my-function
;;;        '("hello" "world" :VERBOSE T
;;;                          :FOO "1" :BAR "2" :ABC "3" :QWE "4"
;;;                          :A "1" :B "2"
;;;                          :X T :Y T :Z T))
;;;
;;; The example above uses mixed option styles
;;; intentionally. Normally, one would only use a single style
;;; according to personal preference.
;;;
;;; Note that ":" or "=" is required when specifying options with
;;; values (keyword arguments). To indicate end-of-options, use "--"
;;; or "::".
;;;
;;; === Long options ===
;;;
;;; $ cmd --abc      -> (:abc t)
;;; $ cmd --abc=100  -> (:abc "100")
;;; $ cmd --abc= 100 -> (:abc "100")
;;; $ cmd --abc=foo=bar -> (:abc "foo=bar")
;;; $ cmd --abc:100  -> (:abc "100")
;;; $ cmd --abc: 100 -> (:abc "100")
;;; $ cmd --abc:foo:bar  -> (:abc "foo:bar")
;;; $ cmd --abc=foo: bar  -> (:abc=foo "bar")
;;;
;;; Long options for non-keyword symbols (rarely needed)
;;;
;;; $ cmd --abc:foo: bar  -> (abc:foo "bar")
;;; $ cmd --abc:foo= bar  -> (abc:foo "bar")
;;;
;;; Note about ambiguity in long options, like:
;;;
;;; "--abc=foo":
;;;   Does it mean (:abc=foo t) or (:abc "foo")?
;;;
;;; "--abc=foo=bar":
;;;   Does it mean (:abc=foo "bar") or (:abc "foo=bar"")?
;;;
;;; "--abc:foo:bar":
;;;   Does it mean (abc:foo "bar") or (:abc "foo:bar"")?
;;;
;;; In cases like these (where the argument does not end in "=" or
;;; ":"), the *first* "=" or ":" encountered is used as key-value
;;; separator. So in the examples above, the latter choices would be
;;; taken.
;;;
;;; === Short options ===
;;;
;;; $ cmd -x         -> (:x t)
;;; $ cmd -x:1       -> (:x "1")
;;; $ cmd -x: 1      -> (:x "1")
;;; $ cmd -x=1       -> (:x "1")
;;; $ cmd -x= 1      -> (:x "1")
;;; $ cmd -xyz       -> (:x t :y t :z t)
;;;
;;; === Keyword-like options ===
;;;
;;; $ cmd :abc 100   -> (:abc "100")
;;;
;;; === End of options ===
;;;
;;; $ cmd -- arg
;;; $ cmd :: arg

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:apprentice)
    (make-package '#:apprentice :use '(#:cl))))

(in-package :apprentice)

(defun keywordify-cli (string)
  (let ((*package* (find-package :keyword))
        (*read-eval* nil))
    ;; May read non-keyword symbols too, that's ok.
    (let ((symbol (read-from-string string)))
      (unless (symbolp symbol)
        (error "Reading ~S did not return a symbol."
               string))
      symbol)))

(defun headp-cli (head-symbol list)
  (and (consp list)
       (eq (car list) head-symbol)))

;; interpret-* entry point
(defun interpret-flexible-option-cli (arg)
  (cond ((or (equal "--" arg)
             (equal "::" arg))
         'end-of-options)
        ((eql 2 (mismatch "--" arg))
         (interpret-flex-long-option-cli arg))
        ((eql 1 (mismatch "-" arg))
         (interpret-flex-short-option-cli arg))
        ((eql 1 (mismatch ":" arg))
         (interpret-flex-keyword-option-cli arg))))

;; ":foo"
(defun interpret-flex-keyword-option-cli (arg)
  (assert (< 1 (length arg)))
  (list :name (read-from-string arg)
        :requires-value t))

;; "-x"
(defun interpret-flex-short-option-cli (arg)
  (assert (< 1 (length arg)))
  (flet ((keywordify-check (char)
           (assert (alphanumericp char))
           (keywordify-cli (string char))))
    (cond ((= 2 (length arg))
           ;; -x
           (list :name (keywordify-check (aref arg 1))
                 :value t))
          ;; -x:
          ((and (= 3 (length arg))
                (member (aref arg 2) '(#\= #\:)))
           (list :name (keywordify-check (aref arg 1))
                 :requires-value t))
          ;; -x=1
          ((and (< 3 (length arg))
                (member (aref arg 2) '(#\= #\:)))
           (list :name (keywordify-check (aref arg 1))
                 :value (subseq arg 3)))
          ;; -xyz
          (t (cons 'flags
                   (map 'list (lambda (x)
                                (list :name (keywordify-check x)
                                      :value t))
                        (subseq arg 1)))))))

;; "--abc"
(defun interpret-flex-long-option-cli (arg)
  (assert (< 2 (length arg)))
  (let ((actual (subseq arg 2)))
    (flet ((keywordify-check (string)
             (assert (< 0 (length string)))
             (keywordify-cli string)))
      (let* ((first-kw-separator-position
               (position-if (lambda (x)
                              (member x '(#\= #\:)))
                            actual))
             (ends-with-kw-separator ;; May not be the first
               (member (aref arg (1- (length arg)))
                       '(#\= #\:))))
        ;;    --abc=, --abc:
        (cond (ends-with-kw-separator
               (list :name (keywordify-check
                            (subseq actual 0 (- (length actual) 1)))
                     :requires-value t))
              ;; --abc=123, --abc:123
              ;; (separator found, but not at the end)
              ((and first-kw-separator-position
                    (not ends-with-kw-separator))
               (list :name (keywordify-check
                            (subseq actual 0 first-kw-separator-position))
                     :value (subseq actual (1+ first-kw-separator-position))))
              ;; --abc
              ((not first-kw-separator-position)
               (list :name (keywordify-check actual)
                     :value t))
              (t (error "Shouldn't have ended up here.")))))))

;;; A command line option reader will take a list of command line
;;; arguments and returns two values. The first value is either a
;;; string or a (possibly empty) list of options, where an option is
;;; of the form (:name :abc :value xyz). The second value is the list
;;; of remaining command line arguments.

(defun make-flexible-option-reader-cli (&key parse-integers)
  (let ((end-of-options nil))
    (flet ((interpret-value (value)
             (if parse-integers
                 (handler-case (parse-integer value)
                   (error () value))
                 value)))
      (lambda (args)
        (if end-of-options
            (values (first args) (rest args))
            (let ((option-or-arg
                    (interpret-flexible-option-cli (first args))))
              ;; Handle the cases: option, non-option or end-of-options.
              (cond
                ;; Non-option argument
                ((null option-or-arg)
                 (values (first args) (rest args)))
                ;; --
                ((eq 'end-of-options
                     option-or-arg)
                 (setf end-of-options t)
                 (values nil (rest args)))
                ;; -xyz
                ((headp-cli 'flags option-or-arg)
                 (values (rest option-or-arg)
                         (rest args)))
                ;; Other option
                ((consp option-or-arg)
                 (destructuring-bind (&key name value requires-value)
                     option-or-arg
                   (assert (or value requires-value))
                   (if value
                       (values (list (list :name name
                                           :value (interpret-value value)))
                               (rest args))
                       (progn
                         (unless (rest args)
                           (error "No value for option ~S." name))
                         (values
                          (list (list :name name
                                      :value (interpret-value (second args))))
                          (cddr args)))))))))))))

(defun parse-options-using-reader-cli (option-reader args)
  (let (options
        non-option-args)
    (loop :with tail = args
          :while tail
          :do (multiple-value-bind (options-or-arg remaining)
                  (funcall option-reader tail)
                (assert (not (eq remaining tail)))
                (setf tail remaining)
                (typecase options-or-arg
                  (null nil)
                  (string (push options-or-arg non-option-args))
                  (cons
                   (dolist (opt options-or-arg)
                     (push (getf opt :name) options)
                     (push (getf opt :value) options))))))
    (append (nreverse non-option-args)
            (nreverse options))))

(defun parse-command-line-options-flexible (args)
  (parse-options-using-reader-cli
   (make-flexible-option-reader-cli)
   args))
