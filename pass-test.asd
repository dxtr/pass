#|
  This file is a part of pass project.
  Copyright (c) 2015 Kim Lidström (kim@dxtr.im)
|#

(in-package :cl-user)
(defpackage pass-test-asd
  (:use :cl :asdf))
(in-package :pass-test-asd)

(defsystem pass-test
  :author "Kim Lidström"
  :license "ISC"
  :depends-on (:pass
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "pass"))))
  :description "Test system for pass"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
