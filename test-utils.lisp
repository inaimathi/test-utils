(in-package #:test-utils)

;;;;; Minor prove-utils
(defmacro a-suite (&rest forms)
  "This hacks around :prove's requirement that a number of forms be provided, and #'prove:finalze be called around each set of :prove tests.
Pointed out at https://github.com/fukamachi/prove/issues/14, but not yet addressed."
  `(progn
     (prove:plan ,(length forms))
     ,@forms
     (prove:finalize)))

(defmacro qchecks (quickcheck-test &optional message)
  "Form for calling quickcheck tests from a prove test (this lets you easily execute :quickcheck properties as part of a prove:run)"
  `(is (quickcheck ,quickcheck-test) t ,message))

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
