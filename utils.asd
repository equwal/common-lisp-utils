(in-package :cl-user)
(defpackage utils-asd
    (:use :cl :asdf))
(in-package :utils-asd)
(defsystem :utils
  :depends-on (:let-over-lambda)
  :components ((:file "utils")))
