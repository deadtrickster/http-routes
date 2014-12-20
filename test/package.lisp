(in-package :cl-user)

(defpackage :http-routes.test
  (:use :cl
        :alexandria
        :stefil))

(in-package :http-routes.test)

(defsuite* (http-routes-tests :in root-suite)
    (run-child-tests))
