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
  (:use :cl :alexandria :http-routes.parser :http-routes.matcher)
  (:export #:make-routes
           #:add-route
           #:try-match-url
           #:*routes*))

;; (defpackage http-routes.helpers
;;   (:use :cl :alexandria :split-sequence :parse-number))

(defpackage http-routes
  (:use :cl :alexandria :http-routes.routes)
  (:shadow #:get
           #:delete
           #:trace)
  (:export #:*route-table*
           #:make-route-table
           #:named-routes
           #:get-named-route
           #:attach-routes-packet
           #:define-routes
           #:reload-all-routes
           #:root
           #:section
           #:add-route
           #:get
           #:head
           #:post
           #:put
           #:delete
           #:trace
           #:options
           #:connect
           #:patch
           #:query
           #:route-method
           #:route-path
           #:route-handler
           #:route-defaults
           #:route-validators
           #:route-path-generator
           #:path-for))
