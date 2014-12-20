(in-package :http-router.router)

(defvar *route-table*)

(defclass route-table ()
  ((get :initform (http-routes.matcher::make-node))
   (head :initform (http-routes.matcher::make-node))
   (post :initform (http-routes.matcher::make-node))
   (put :initform (http-routes.matcher::make-node))
   (delete :initform (http-routes.matcher::make-node))
   (trace :initform (http-routes.matcher::make-node))
   (options :initform (http-routes.matcher::make-node))
   (connect :initform (http-routes.matcher::make-node))
   (patch :initform (http-routes.matcher::make-node))))

(defclass route ()
  ((methods :accessor route-method :initarg :methods)
   (path :accessor route-path :initarg :path)
   (handler :accessor route-handler :initarg :handler)
   (defaults :accessor route-defaults :initarg :defaults)
   (validators :accessor route-validators :initarg :validators)))

(defmethod attach-routes-packet (system)
  (declare (ignore system))
  nil)

(defmacro define-routes (name &body routes)
  (let ((include-name (intern "INCLUDE" *package*)))
    `(macrolet ((,include-name (packet-to-include)
                  `(progn
                     (attach-routes-packet ,packet-to-include))))
       (defmethod attach-routes-packet ((system (eql ,name)))
         (let ((*route-section* ""))
           ,@routes)))))

(defmethod route-table-routes ((method (eql :get)))
  (slot-value *route-table* 'get))
(defmethod route-table-routes ((method (eql :post)))
  (slot-value *route-table* 'post))
(defmethod route-table-routes ((method (eql :put)))
  (slot-value *route-table* 'put))
(defmethod route-table-routes ((method (eql :delete)))
  (slot-value *route-table* 'delete))
(defmethod route-table-routes ((method (eql :trace)))
  (slot-value *route-table* 'trace))
(defmethod route-table-routes ((method (eql :options)))
  (slot-value *route-table* 'options))
(defmethod route-table-routes ((method (eql :connect)))
  (slot-value *route-table* 'connect))
(defmethod route-table-routes ((method (eql :patch)))
  (slot-value *route-table* 'patch))

(defun add-route-to-routes (routes path route)
  (let ((*routes* routes))
    (add-route path route)))

(defun add-route% (route-table path &key handler methods defaults validators)
  (unless methods
    (setf methods '(:get)))
  (let ((route (make-instance 'route
                              :methods methods
                              :handler handler
                              :path path
                              :defaults defaults
                              :validators validators)))
    (loop for method in methods
          as routes = (route-table-routes method) do
          (add-route-to-routes routes path route))))

(defmacro route (uri &key handler methods as defaults validators)
  (let ((path-function (if as
                           `(setf (symbol-function ',as)
                                  (lambda () ;; just return path for now
                                    path))))) 
    `(let ((path (concatenate 'string *route-section* ,uri)))
       (add-route% *route-table* path :to ,handler :methods ,methods :defaults ,defaults :validators ,validators)
       ,path-function)))

(defun add-route (path handler)
  (add-route% *route-table* path :to handler :method :get))

(defmacro root (&key handler as)
  `(route "/" :to ,handler :as ,as))

(defmacro section (sub-path &body routes)
  `(let ((*route-section* (if *route-section*
                              (concatenate 'string *route-section* ,sub-path)
                              ,sub-path)))
     ,@routes))

(defun attach-all-routes (system)
  (let ((deps (get-all-deps system)))
    (loop for dep in (reverse deps) do
             (attach-system-routes dep))))

(defun reload-all-routes (system-name)
  (setf *route-table* (make-instance 'route-table))
  (attach-routes-packet system-name))

;; example
#|
(define-routes :admin
  (section "/admin"
    (route "/login" :methods :post :handler 'do-login)
    (route "/logout" :handler 'do-logout)))

(define-routes :my-app
  (include :admin)
  (route "/*path" :handler 'render-page)
  (route "/forum/:|topic-name|-:topic-id"))

#|
