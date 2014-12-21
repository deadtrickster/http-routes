(in-package :cl-user)

(defpackage :http-routes.matcher
  (:use :cl :alexandria)
  (:export #:make-node
           #:create-pattern
           #:add-to-tree
           #:match))

(defpackage http-routes.parser
  (:use :cl :alexandria)
  (:export #:parse-route
           #:route-variables
           #:route-to-match-rules))

(defpackage http-routes.routes
  (:use :cl :alexandria :http-routes.parser :Http-routes.matcher)
  (:export #:make-routes
           #:add-route
           #:try-match-url
           #:*routes*))

(defpackage http-routes.helpers
  (:use :cl :alexandria :split-sequence :parse-number))

(defpackage http-routes
  (:use :cl :alexandria :http-routes.router))
