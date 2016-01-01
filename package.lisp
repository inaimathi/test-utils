;;;; package.lisp

(defpackage #:test-utils
  (:use #:cl #:prove #:cl-quickcheck)
  (:shadowing-import-from #:prove #:is #:isnt)
  (:export
   #:a-suite #:qchecks

   #:a-boolean #:a-string #:a-symbol #:a-char #:a-ratio #:a-number #:a-real #:an-atom
   #:an-index #:a-list #:a-member #:a-tuple

   ;; re-export :quickcheck symbols
   #:quickcheck #:is= #:isnt= #:should-signal
   #:named #:wrap-each #:only-if #:for-all
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
