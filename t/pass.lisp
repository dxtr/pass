(in-package :cl-user)
(defpackage pass-test
  (:use :cl
        :pass
        :prove))
(in-package :pass-test)

;; NOTE: To run this test file, execute `(asdf:test-system :pass)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
