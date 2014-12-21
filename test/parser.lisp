(in-package :http-routes.test)

(in-suite :http-routes.parser)

(test (parse-route-test :compile-at :definition-time)
  (is-true (equalp (http-routes.routes::parse-route "/orders") '("/orders")))
  (is-true (equalp (http-routes.routes::parse-route "/orders/:order-id") '("/orders/" :order-id)))
  (is-true (equalp (http-routes.routes::parse-route "/orders/:order-id/type") '("/orders/" :order-id "/type")))
  (is-true (equalp (http-routes.routes::parse-route "/control(/*path)") '("/control" (&optional "/" &rest :path))))
  (is-true (equalp (http-routes.routes::parse-route "/do/movers(/:mover-id)(/:tab)(/:sub-id)(/*rest)") '("/do/movers" (&optional "/" :mover-id)
                                                                                                         (&optional "/" :tab)
                                                                                                         (&optional "/" :sub-id)
                                                                                                         (&optional "/" &rest :rest))))
  (is-true (equalp (http-routes.routes::parse-route "/blog/*path") '("/blog/" &rest :path)))
  (is-true (equalp (http-routes.routes::parse-route "/client(/)*path") '("/client" (&optional "/") &rest :path)))
  (is-true (equalp (http-routes.routes::parse-route "/forum/:|topic-title|-:topic-id") '("/forum/" :topic-title "-" :topic-id))))

(test (route-variables-test :compile-at :definition-time)
  (flet ((route-variables% (string)
           (http-routes.routes::route-variables (http-routes.routes::parse-route string))))
    (is-true (equalp (route-variables% "/orders") '()))
    (is-true (equalp (route-variables% "/orders/:order-id") '((:segment :order-id))))
    (is-true (equalp (route-variables% "/orders/:order-id/type") '((:segment :order-id))))
    (is-true (equalp (route-variables% "/control(/*path)") '((:multi-segment :path))))
    (is-true (equalp (route-variables% "/do/movers(/:mover-id)(/:tab)(/:sub-id)(/*rest)") '((:segment :MOVER-ID)
                                                                                           (:segment :TAB)
                                                                                           (:segment :SUB-ID)
                                                                                           (:multi-segment :REST))))
    (is-true (equalp (route-variables% "/blog/*path") '((:multi-segment :path))))
    (is-true (equalp (route-variables% "/client(/)*path") '((:multi-segment :path))))))

(test (route-to-match-rules-test :compile-at :definition-time)
  (flet ((route-to-match-rules% (route)
           (http-routes.routes::route-to-match-rules (http-routes.routes::parse-route route))))
    (is-true (equalp (route-to-match-rules% "/orders") '(("/orders"))))
    (is-true (equalp (route-to-match-rules% "/orders/:order-id") '(("/orders/" :wildcard))))
    (is-true (equalp (route-to-match-rules% "/orders/:order-id/type") '(("/orders/" :wildcard "/type"))))
    (is-true (equalp (route-to-match-rules% "/control(/*path)") '(("/control") ("/control" "/" :wildcard))))
    (is-true (equalp (route-to-match-rules% "/blog/*path") '(("/blog/" :wildcard))))
    (is-true (equalp (route-to-match-rules% "/do/movers(/:mover-id)(/:tab)(/:sub-id)(/*rest)")
                    `(("/do/movers")
                      ("/do/movers" "/" :wildcard)
                      ("/do/movers" "/" :wildcard "/" :wildcard)
                      ("/do/movers" "/" :wildcard "/" :wildcard "/" :wildcard)
                      ("/do/movers" "/" :wildcard "/" :wildcard "/" :wildcard "/" :wildcard))))
    (is-true (equalp (route-to-match-rules% "/orders/(id):order-id") '(("/orders/" :wildcard)
                                                                      ("/orders/" "id" :wildcard))))))