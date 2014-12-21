(in-package :http-routes.test)

(in-suite :http-routes.route)

(defmacro route-match (url tag parameters)
  `(multiple-value-bind (tag parameters)
       (http-routes.routes:try-match-url ,url)
     (is-true (equalp tag ,tag))
     (is-true (equalp parameters ,(alexandria:alist-hash-table parameters)))))

(test (full-route-test :compile-at :definition-time)
  (let ((http-routes.routes:*routes* (http-routes.routes:make-routes)))
    (http-routes.routes:add-route "/articles/*path" :article-wildcard)
    (http-routes.routes:add-route "/articles/some-article" :simple-static-route)
    (http-routes.routes:add-route "/*a/foo/*b" :wild-wildcards)

    (route-match "/articles/some-article"
                 :simple-static-route nil)
    (route-match "/articles/very-important/thing-happened"
                 :article-wildcard ((:path . "very-important/thing-happened")))
    (route-match "/zoo/woo/foo/bar/baz"
                 :wild-wildcards ((:a . "zoo/woo")
                                  (:b . "bar/baz")))))

