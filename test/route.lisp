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
    (http-routes.routes:add-route "/orders/:order-id" :order-with-id)
    (http-routes.routes:add-route "/orders/:order-id/name" :order-with-id-and-name)
    (http-routes.routes:add-route "/client/(id):client-id" :client-with-maybe-old-id)
    (http-routes.routes:add-route "foo/*wildcard/bar" :wildcard-in-the-middle)
    (http-routes.routes:add-route "/blog(/)(*path)" :blog-with-index-and-path)

    (route-match "/articles/some-article"
                 :simple-static-route nil)
    (route-match "/articles/very-important/thing-happened"
                 :article-wildcard ((:path . "very-important/thing-happened")))
    (route-match "/zoo/woo/foo/bar/baz"
                 :wild-wildcards ((:a . "zoo/woo")
                                  (:b . "bar/baz")))
    (route-match "/orders/123"
                 :order-with-id ((:order-id . "123")))
    (route-match "/orders/123/name"
                 :order-with-id-and-name ((:order-id . "123")))
    (route-match "/orders/12/3/name"  ;; multi segment order-id 
                 nil nil)
    (route-match "/client/123"
                 :client-with-maybe-old-id ((:client-id . "123")))    
    (route-match "/client/id123"
                 :client-with-maybe-old-id ((:client-id . "123")))
    (route-match "foo/woo/foo/bar"
                 :wildcard-in-the-middle ((:wildcard . "woo/foo")))    
    (route-match "/blog"
                 :blog-with-index-and-path nil)    
    (route-match "/blog/"
                 :blog-with-index-and-path nil)    
    (route-match "/blog/loop-for-black-belts"
                 :blog-with-index-and-path ((:path . "loop-for-black-belts")))))

