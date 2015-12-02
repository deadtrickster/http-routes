http-routes  [![Build Status](https://travis-ci.org/deadtrickster/http-routes.svg?branch=master)](https://travis-ci.org/deadtrickster/http-routes) [![Coverage Status](https://coveralls.io/repos/deadtrickster/http-routes/badge.svg?branch=master&service=github)](https://coveralls.io/github/deadtrickster/http-routes?branch=master)
===========
An attempt to create capabale http(url) routes lib without regexps, very early stage, 

Example route definitions

```lisp
(define-routes :api  
  ;;pages
  (get "/pages(/)" :handler "get pages")
  (post "/pages(/)" :handler "create new page")
  (get "/pages/:page-id" :handler "get page" :name 'api-page-path)
  (delete "/pages/:page-id" :handler "delete page")
  (post "/pages/:page-id" :handler "update page"))

(define-routes :admin ;; this equals to (:admin "/admin")
  (include :api)
  ;;service stuff
  (get "/login" :handler "default admin login page" :name 'admin-login-path)
  (post "/login" :handler "perform actual admin login")
  (get "/logout" :handler "admin logout")
  ;;main route for our Single Page Application
  (get "(/)(*spa-path)" :handler "admin spa"))

(define-routes (:site "")
  (include :admin :section "/cms-admin")
  (get "/(*page-id)" :handler "render page" :defaults (alist-hash-table '((:page-id . :latest))))
  (get "/cms-admin/login" :handler "site customized admin login page"))
```
after attaching these routes:
```lisp
(path-for 'admin-login-path)
"/cms-admin/login"
(path-for 'api-page-path '((:page-id . 123)))
"/api/pages/123"
```
