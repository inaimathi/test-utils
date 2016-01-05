;;;; prove-utils.asd

(asdf:defsystem #:test-utils
  :description "Convenience functions and macros for testing Common Lisp applications via Prove and Quickcheck"
  :author "inaimathi <leo.zovic@gmail>"
  :license "MIT Expat <http://directory.fsf.org/wiki/License:Expat>"
  :depends-on (#:alexandria #:prove #:cl-quickcheck)
  :serial t
  :components ((:file "package")
               (:file "test-utils")))
