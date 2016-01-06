(in-package #:test-utils)

;;;;; Minor prove-utils
(defmacro tests (&rest forms)
  "This hacks around :prove's requirement that a number of forms be provided, and #'prove:finalze be called around each set of :prove tests.
Pointed out at https://github.com/fukamachi/prove/issues/14, but not yet addressed."
  `(progn
     (prove:plan ,(length forms))
     ,@forms
     (prove:finalize)))

(defmacro quiet-check (&body body)
  "Like :quickcheck, but squelches the initial seed reporting. Useful for running quickcheck properties in the middle of :prove suites."
  (with-gensyms (res msg short-msg)
    `(let ((,res nil)
	   (,msg nil))
       (setf ,msg (with-output-to-string (*standard-output*)
		    (setf ,res (quickcheck ,@body))))
       (unless ,res
	 (let ((,short-msg (subseq ,msg (nth-value 1 (read-from-string ,msg nil nil :start 25)))))
	   (format t "~a" ,short-msg)))
       ,res)))

(defmacro qchecks (quickcheck-test &optional message)
  "Form for calling quickcheck tests from a prove test (this lets you easily execute :quickcheck properties as part of a prove:run)"
  `(is (quiet-check ,quickcheck-test) t ,message))

(defmacro for-all ((&rest bindings) test &optional message)
  `(qchecks
    (cl-quickcheck:for-all ,bindings ,test)
    ,(or message (write-to-string test))))

;;;;; Make namespacing consistent amongst primitive quickcheck generators
(defparameter a-string #'cl-quickcheck:a-string)
(defparameter a-symbol #'cl-quickcheck:a-symbol)
(defparameter a-char #'cl-quickcheck:a-char)

;;;;; New quickcheck generators
(defparameter a-ratio
  (lambda () (rationalize (generate a-real))))

(defparameter a-number
  (a-member an-integer a-real a-ratio))

(defparameter an-atom
  (a-member a-number a-boolean a-string a-symbol a-char))

(defun a-vector (generator)
  (lambda () (coerce (generate (a-list generator)) 'vector)))

(defun a-pair (a-generator b-generator)
  (lambda () (cons (generate a-generator) (generate b-generator))))

(defun a-hash (key-generator value-generator)
  (lambda ()
    (let ((res (make-hash-table :test 'equalp)))
      (loop repeat (generate an-index)
	 for k = (generate key-generator)
	 for v = (generate value-generator)
	 do (setf (gethash k res) v))
      res)))

(defparameter a-value
  (a-member an-atom (a-pair an-atom an-atom) (a-list an-atom) (a-vector an-atom) (a-hash an-atom an-atom)))
