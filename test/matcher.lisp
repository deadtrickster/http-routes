(in-package :http-routes.test)

(in-suite :http-routes.matcher)

(defun create-test-match-tree ()
  (let ((root (http-routes.matcher:make-node)))
    (http-routes.matcher:add-to-tree root "qwe" 1)
    (http-routes.matcher:add-to-tree root "qwa" 2)
    (http-routes.matcher:add-to-tree root "control" :control-index)
    (http-routes.matcher:add-to-tree root (http-routes.matcher:create-pattern '("control/" :path)) :control-wildcard)
    (http-routes.matcher:add-to-tree root "control/login" :control-login)
    (http-routes.matcher:add-to-tree root "orders" :orders)
    (http-routes.matcher:add-to-tree root (http-routes.matcher:create-pattern '("orders/" :order-id)) :order-id)
    (http-routes.matcher:add-to-tree root (http-routes.matcher:create-pattern '("orders/" :order-id "/history")) :order-id-history)
    (http-routes.matcher:add-to-tree root (http-routes.matcher:create-pattern '("orders/" :order-id "/history/" :item-id)) :order-id-history-item-id)
    (http-routes.matcher:add-to-tree root (http-routes.matcher:create-pattern '("orders/" :order-id "/history/" :item-id "/name")) :order-id-history-item-id-name)
    (http-routes.matcher:add-to-tree root "orders/latest" :latest-order)
    (http-routes.matcher:add-to-tree root "orders/latest/history" :latest-order-history)
    (http-routes.matcher:add-to-tree root "orders/latest/history/latest" :latest-order-latest-history-item)
    (http-routes.matcher:add-to-tree root "orders/latest/history/latest/name" :latest-order-latest-history-item-name)
    (http-routes.matcher:add-to-tree root "/blog/edit" "/blog/edit")
    (http-routes.matcher:add-to-tree root (http-routes.matcher:create-pattern '("/blog/" :blog-path)) "/blog/*blog-path")
    root))

(test (matcher.1 :compile-at :definition-time)
  (let ((root (create-test-match-tree)))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "qwe")
      (declare (ignore index))
      (is-true (eql matched t))
      (is-true (eql tag 1)))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "qwa")
      (declare (ignore index))
      (is-true (eql matched t))
      (is-true (eql tag 2)))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "qwezw")
      (is-true (eql matched nil))
      (is-true (eql tag 1))
      (is-true (eql index 3)))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "qw")
      (declare (ignore index))
      (is-true (eql matched t))
      (is-true (eql tag nil)))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "control")
      (declare (ignore index))
      (is-true (eql matched t))
      (is-true (eql tag :control-index)))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "control/")
      (declare (ignore index))
      (is-true (eql matched t))
      (is-true (eql tag nil)))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "control/do/orders")
      (is-true (eql matched :wildcard))
      (is-true (eql tag :control-wildcard))
      (is-true (equalp index '((:path 8)))))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "control/logout")
      (is-true (eql matched :wildcard))
      (is-true (eql tag :control-wildcard))
      (is-true (equalp index '((:path 8)))))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "control/login")
      (declare (ignore index))
      (is-true (eql matched t))
      (is-true (eql tag :control-login)))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "orders")
      (declare (ignore index))
      (is-true (eql matched t))
      (is-true (eql tag :orders)))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "orders/123")
      (is-true (eql matched :wildcard))
      (is-true (eql tag :order-id))
      (is-true (equalp index '((:order-id 7)))))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "orders/123/history")
      (is-true (eql matched :wildcard))
      (is-true (eql tag :order-id-history))
      (is-true (equalp index '((:order-id 7 10)))))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "orders/123/history/456")
      (is-true (eql matched :wildcard))
      (is-true (eql tag :order-id-history-item-id))
      (is-true (equalp index '((:order-id 7 10) (:item-id 19)))))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "orders/123/history/456/name")
      (is-true (eql matched :wildcard))
      (is-true (eql tag :order-id-history-item-id-name))
      (is-true (equalp index '((:order-id 7 10) (:item-id 19 22)))))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "orders/qwe/qwe/qwe")
      (is-true (eql matched :wildcard))
      (is-true (eql tag :order-id))
      (is-true (equalp index '((:order-id 7)))))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "orders/latest")
      (declare (ignore index))
      (is-true (eql matched t))
      (is-true (eql tag :latest-order)))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "orders/latest/history")
      (declare (ignore index))
      (is-true (eql matched t))
      (is-true (eql tag :latest-order-history)))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "orders/latest/history/latest")
      (declare (ignore index))
      (is-true (eql matched t))
      (is-true (eql tag :latest-order-latest-history-item)))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "orders/latest/history/latest/name")
      (declare (ignore index))
      (is-true (eql matched t))
      (is-true (eql tag :latest-order-latest-history-item-name)))    
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "/blog/edit")
      (declare (ignore index))
      (is-true (eql matched t))
      (is-true (equal tag "/blog/edit")))
    (multiple-value-bind (matched tag index) (http-routes.matcher:match root "/blog/ed")
      (declare (ignore index))
      (is-true (eql matched :wildcard))
      (is-true (equal tag "/blog/*blog-path")))))
