(in-package #:test-utils)

;;;;; Minor prove-utils
(defmacro a-suite (&rest forms)
  "This hacks around :prove's requirement that a number of forms be provided, and #'prove:finalze be called around each set of :prove tests.
Pointed out at https://github.com/fukamachi/prove/issues/14, but not yet addressed."
  `(progn
     (prove:plan ,(length forms))
     ,@forms
     (prove:finalize)))

(defmacro quiet-check (&body body)
  "Like :quickcheck, but squelches the initial seed reporting. Useful for running quickcheck properties in the middle of :prove suites."
  (let ((res (gensym))
	(msg (gensym))
	(short-msg (gensym)))
    `(let ((,res nil)
	   (,msg nil))
       (setf ,msg (with-output-to-string (*standard-output*)
		    (setf ,res (quickcheck ,@body))))
       (let ((,short-msg (subseq ,msg (nth-value 1 (read-from-string ,msg nil nil :start 25)))))
	 (unless ,res (format t "~a" ,short-msg))
	 ,res))))

(defmacro qchecks (quickcheck-test &optional message)
  "Form for calling quickcheck tests from a prove test (this lets you easily execute :quickcheck properties as part of a prove:run)"
  `(is (quiet-check ,quickcheck-test) t ,message))

;;;;; Make namespacing consistent amongst primitive quickcheck generators
(defparameter a-string #'cl-quickcheck:a-string)
(defparameter a-symbol #'cl-quickcheck:a-symbol)
(defparameter a-char #'cl-quickcheck:a-char)

;;;;; New quickcheck generators
(defparameter a-ratio
  (lambda () (rationalize (funcall a-real))))

(defparameter a-number
  (a-member an-integer a-real a-ratio))

(defparameter an-atom
  (a-member a-number a-boolean a-string a-symbol a-char))

(defun a-vector (generator)
  (lambda ()
    (coerce (funcall (a-list generator)) 'vector)))

(defun a-hash (key-generator value-generator)
  (lambda ()
    (let ((res (make-hash-table :test 'equalp)))
      (loop repeat (funcall an-index)
	 for k = (funcall key-generator)
	 for v = (funcall value-generator)
	 do (setf (gethash k res) v))
      res)))

(defparameter a-value
  (a-member an-atom (a-list an-atom) (a-vector an-atom) (a-hash an-atom an-atom)))
