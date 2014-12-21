(in-package :cl-user)

(defpackage :http-routes.test
  (:use :cl
        :alexandria
        :5am))

(in-package :http-routes.test)

(def-suite :http-routes
  :description "Main test suite for HTTP-ROUTES")

(def-suite :http-routes.matcher
  :description "HTTP-ROUTES Matcher tests")

(def-suite :http-routes.parser
  :description "HTTP-ROUTES Parser tests")
