http-routes  [![Build Status](https://travis-ci.org/deadtrickster/http-routes.svg?branch=master)](https://travis-ci.org/deadtrickster/http-routes)
===========
An attempt to create capabale http(url) routes lib without regexps, very early stage, 

Example route definitions

```lisp
(define-routes :api  
  (section "/api"
    ;;pages
    (get "/pages(/)" :handler "get pages")
    (post "/pages(/)" :handler "create new page")
    (get "/pages/:page-id" :handler "get page")
    (delete "/pages/:page-id" :handler "delete page")
    (post "/pages/:page-id" :handler "update page")))

(define-routes :admin
  (include :api)
  (section "/admin"
    ;;service stuff
    (get "/login" :handler "default admin login page")
    (post "/login" :handler "perform actual admin login")
    (get "/logout" :handler "admin logout")
    ;;main route for our Single Page Application
    (get "(/)(*spa-path)" :handler "admin spa")))

(define-routes :site
  (include :admin)
  (get "/(*page-id)" :handler "render page" :defaults (alist-hash-table '((:page-id . :latest))))
  (get "/admin/login" :handler "site customized admin login page"))
```
