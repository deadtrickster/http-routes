(in-package :cl-user)

(defpackage :http-routes.matcher
  (:use :cl :alexandria)
  (:export :create-patterns-bag
           :add-pattern
           :remove-patter
           :match-pattern))

(defpackage http-routes.routes
  (:use :cl :alexandria :http-routes.matcher))

(defpackage http-routes.router
  (:use :cl :alexandria :http-routes.routes))

(defpackage http-routes.helpers
  (:use :cl :alexandria :split-sequence :parse-number))

(defpackage http-routes
  (:use :cl :alexandria :http-routes.router))
