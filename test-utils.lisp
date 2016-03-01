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
(defun one-of (&rest elems)
  (let ((es (coerce elems 'vector)))
    (lambda ()
      (aref es (random (length es))))))

(defparameter a-ratio
  (lambda () (rationalize (generate a-real))))

(defparameter a-number
  (a-member an-integer a-real a-ratio))

(defparameter a-keyword
  (lambda ()
    (intern (generate a-string) :keyword)))

(defun an-alist (key-generator value-generator)
  (a-list (a-pair key-generator value-generator)))

(defun a-plist (key-generator value-generator)
  (lambda ()
    (loop repeat (generate an-index)
       collect (generate key-generator)
       collect (generate value-generator))))

(defun an-improper-list (generator)
  (lambda ()
    (let ((lst (generate (a-pair generator generator))))
      (loop repeat (generate an-index)
	 do (setf lst (cons (generate generator) lst)))
      lst)))

(defun a-vector (generator)
  (lambda () (coerce (generate (a-list generator)) 'vector)))

(defun an-array (dimensions generator)
  (if (null dimensions)
      (lambda () (make-array nil))
      (labels ((n-tuple (n gen) (lambda () (loop repeat n collect (generate gen))))
	       (chain (ds gen)
		 (if (null ds)
		     gen
		     (chain (cdr ds) (n-tuple (car ds) gen)))))
	(let* ((ds (reverse dimensions))
	       (gen (chain (cdr ds) (n-tuple (car ds) generator)) ))
	  (lambda ()
	    (make-array dimensions :initial-contents (generate gen)))))))

(defun a-hash (key-generator value-generator)
  (lambda ()
    (let ((res (make-hash-table :test 'equalp)))
      (loop repeat (generate an-index)
	 for k = (generate key-generator)
	 for v = (generate value-generator)
	 do (setf (gethash k res) v))
      res)))

(defun a-specific-hash (&rest k/gen-pairs)
  (lambda ()
    (let ((h (make-hash-table :test 'equalp)))
      (loop for (k gen) on k/gen-pairs by #'cddr
	 do (setf (gethash k h) (generate gen)))
      h)))

(defun a-specific-plist (&rest k/gen-pairs)
  (lambda ()
    (loop for (k gen) on k/gen-pairs by #'cddr
       collect k collect (generate gen))))

(defun a-specific-alist (&rest k/gen-pairs)
  (lambda ()
    (loop for (k gen) on k/gen-pairs by #'cddr
       collect (cons k (generate gen)))))

(defparameter an-atom
  (let ((primitive-atom (a-member a-number a-boolean a-string a-symbol a-char)))
    (a-member a-number a-boolean a-string a-symbol a-char
	      (a-vector primitive-atom)
	      (a-hash primitive-atom primitive-atom))))

(defun a-pair (a-generator b-generator)
  (lambda () (cons (generate a-generator) (generate b-generator))))

(defparameter a-value
  (a-member an-atom (a-pair an-atom an-atom) (a-list an-atom)))
