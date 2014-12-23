(in-package :http-routes.test)

(in-suite :http-routes.packet)

(http-routes:define-routes :api  
  ;;pages
  (http-routes:get "/pages(/)" :handler "get pages")
  (http-routes:post "/pages(/)" :handler "create new page")
  (http-routes:get "/pages/:page-id" :handler "get page" :name :api-page-path)
  (http-routes:delete "/pages/:page-id" :handler "delete page")
  (http-routes:post "/pages/:page-id" :handler "update page"))

(http-routes:define-routes :admin ;; this equal to (:admin "/admin")
  (include :api)
  ;;service stuff
  (http-routes:get "/login" :handler "default admin login page" :name :admin-login-path)
  (http-routes:post "/login" :handler "perform actual admin login")
  (http-routes:get "/logout" :handler "admin logout")
  ;;main route for our Single Page Application
  (http-routes:get "(/)(*spa-path)" :handler "admin spa" :name :admin-spa-path))

(http-routes:define-routes (:site "")
  (include :admin :section "/secure-admin")
  (http-routes:get "/(*page-id)" :handler "render page" :defaults (alexandria:alist-hash-table '((:page-id . :latest))))
  (http-routes:get "/secure-admin/login" :handler "site customized admin login page"))

(defmacro test-url-success (method url handler parameters)
  `(multiple-value-bind (route parameters) (http-routes:query ,method ,url)
     (is-true route)
     (is-true (equalp (http-routes:route-handler route)
                      ,handler))
     (is-true (equalp parameters
                      (alexandria:alist-hash-table ',parameters)))))

(defmacro test-url-404 (method url)
  `(multiple-value-bind (route parameters) (http-routes:query ,method ,url)
       (declare (ignore parameters))
       (is-false route)))

(defun attach-site-routes ()
  (http-routes:attach-routes-packet :site))

(test (site-routes :compile-at :definition-time)
  (let ((http-routes:*route-table* (http-routes:make-route-table)))
    (attach-site-routes)
    
    (test-url-success :get "/some-page/qwe/qwe"
                      "render page" ((:page-id . "some-page/qwe/qwe")))
    
    (test-url-404 :post "/some-page/qwe/qwe")
    
    (test-url-success :get "/"
                      "render page" ((:page-id . :latest)))
    
    (test-url-success :get "/secure-admin/login"
                      "site customized admin login page" nil)
    
    (test-url-success :get "/secure-admin/logout"
                      "admin logout" nil)

    (test-url-success :post "/secure-admin/login"
                      "perform actual admin login" nil)

    (test-url-success :get "/secure-admin"
                      "admin spa" nil)
    (test-url-success :get "/secure-admin/"
                      "admin spa" nil)
    (test-url-success :get "/secure-admin/pages"
                      "admin spa" ((:spa-path . "pages")))

    (test-url-success :get "/api/pages"
                      "get pages" nil)
    (test-url-success :get "/api/pages/"
                      "get pages" nil)

    (test-url-success :post "/api/pages"
                      "create new page" nil)
    (test-url-success :post "/api/pages/"
                      "create new page" nil)
    
    (test-url-success :get "/api/pages/123"
                      "get page" ((:page-id . "123")))
    (test-url-404 :get "/api/pages/multi/segment")

    (test-url-success :delete "/api/pages/123"
                      "delete page" ((:page-id . "123")))

    (test-url-success :post "/api/pages/123"
                      "update page" ((:page-id . "123")))
    
    (is-true (http-routes:path-for :api-page-path '((:page-id . 123))) "/api/page/123")
    (is-true (http-routes:path-for :admin-login-path) "/secure-admin/login")
    (is-true (http-routes:path-for :admin-spa-path '((:spa-path . "/pages"))) "/secure-admin/pages")))
