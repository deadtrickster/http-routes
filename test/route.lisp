(in-package :http-routes.test)

(in-suite :http-routes.route)

(defmacro route-match (url tag parameters)
  `(multiple-value-bind (tag parameters)
       (http-routes.routes:try-match-url ,url)
     (is-true (equalp tag ,tag))
     (is-true (equalp parameters ,parameters))))

(test (full-route-test :compile-at :definition-time)
  (let ((http-routes.routes:*routes* (http-routes.routes:make-routes)))
    (http-routes.routes:add-route "/articles/some-article" :simple-static-route)

    (route-match "/articles/some-article"
                        :SIMPLE-STATIC-ROUTE (make-hash-table))))

