;;;; package.lisp

(defpackage #:test-utils
  (:use #:cl #:prove #:cl-quickcheck)
  (:import-from #:alexandria #:with-gensyms)
  (:shadowing-import-from #:prove #:is #:isnt)
  (:shadow #:for-all)
  (:export
   #:tests #:qchecks #:quiet-check #:for-all

   #:a-boolean #:a-string #:a-symbol #:a-char #:an-integer #:a-ratio #:a-number #:a-real #:an-atom #:a-value
   #:an-index  #:a-pair #:a-vector #:a-hash #:a-list #:a-member #:a-tuple
   #:one-of

   ;; re-export :quickcheck symbols
   #:quickcheck #:is= #:isnt= #:should-signal
   #:named #:wrap-each #:only-if
   #:k-generator #:m-generator #:n-generator
   #:define #:generate #:pick-weighted
   #:*testing* #:*break-on-failure* #:*loud* #:*num-trials* #:*size*
   #:test-name #:test-flopped #:test-detail #:test-bindings

   ;; re-export :prove symbols
   #:*debug-on-error*
   #:*test-result-output*
   #:*default-test-function*
   #:*default-reporter*
   #:test-file
   #:run-test-system
   #:run
   #:ok
   #:is
   #:isnt
   #:is-values
   #:is-print
   #:is-error
   #:is-type
   #:like
   #:is-expand
   #:diag
   #:skip
   #:pass
   #:fail
   #:subtest
   #:deftest
   #:run-test
   #:run-test-package
   #:run-test-all
   #:remove-test
   #:remove-test-all
   #:plan
   #:finalize
   #:*gensym-prefix*
   #:*default-slow-threshold*
   #:slow-threshold
   #:current-suite
   #:*suite*
   #:reset-suite
   #:suite
   #:package-suite
   #:*enable-colors*))
