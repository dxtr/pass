#|
  This file is a part of pass project.
  Copyright (c) 2015 Kim Lidström (kim@dxtr.im)
|#

#|
  Author: Kim Lidström (kim@dxtr.im)
|#

(defsystem :pass
  :version "0.1"
  :author "Kim Lidström"
  :license "ISC"
  :depends-on (:getopt :cl-ppcre :getopt :utilities.print-tree)
  :components ((:file "pass"))
  :pathname "src/"
  :description "lulz"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq))))
