(in-package :http-routes)

(defvar *route-table*)

(defvar *route-section* "")

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

(defun make-route-table ()
  (make-instance 'route-table))

(defclass route ()
  ((methods :accessor route-method :initarg :methods)
   (path :accessor route-path :initarg :path)
   (handler :accessor route-handler :initarg :handler)
   (defaults :accessor route-defaults :initarg :defaults)
   (validators :accessor route-validators :initarg :validators)))

(defmethod attach-routes-packet (system &key section)
  (declare (ignore system))
  nil)

(defmacro define-routes (name &body routes)
  (destructuring-bind (name section) (if (listp name)
                                         name
                                         (list name (concatenate 'string "/" (string-downcase (symbol-name name)))))
    (let ((include-name (intern "INCLUDE" *package*)))
      `(macrolet ((,include-name (packet-to-include)
                    `(progn
                       (attach-routes-packet ,packet-to-include))))
         (defmethod attach-routes-packet ((system (eql ,name)) &key (section ,section))
           (let ((*route-section* section))
             ,@routes))))))

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
    (http-routes.routes:add-route path route)))

(defun add-route% (path handler methods defaults validators)
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
       (add-route% path ,handler ,methods  ,defaults ,validators)
       ,path-function)))

(defun get (uri &key handler as defaults validators)
  (route uri :handler handler :methods '(:get) :as as :defaults defaults :validators validators))

(defun head (uri &key handler as defaults validators)
  (route uri :handler handler :methods '(:head) :as as :defaults defaults :validators validators))

(defun post (uri &key handler as defaults validators)
  (route uri :handler handler :methods '(:post) :as as :defaults defaults :validators validators))

(defun put (uri &key handler as defaults validators)
  (route uri :handler handler :methods '(:put) :as as :defaults defaults :validators validators))

(defun delete (uri &key handler as defaults validators)
  (route uri :handler handler :methods '(:delete) :as as :defaults defaults :validators validators))

(defun trace (uri &key handler as defaults validators)
  (route uri :handler handler :methods '(:trace) :as as :defaults defaults :validators validators))

(defun options (uri &key handler as defaults validators)
  (route uri :handler handler :methods '(:options) :as as :defaults defaults :validators validators))

(defun connect (uri &key handler as defaults validators)
  (route uri :handler handler :methods '(:connect) :as as :defaults defaults :validators validators))

(defun patch (uri &key handler as defaults validators)
  (route uri :handler handler :methods '(:patch) :as as :defaults defaults :validators validators))


(defmacro root (&key handler as)
  `(route "/" :to ,handler :as ,as))

(defmacro section (sub-path &body routes)
  `(let ((*route-section* (if *route-section*
                              (concatenate 'string *route-section* ,sub-path)
                               ,sub-path)))
     ,@routes))

;; (defun attach-all-routes (system)
;;   (let ((deps (get-all-deps system)))
;;     (loop for dep in (reverse deps) do
;;              (attach-system-routes dep))))

(defun reload-all-routes (system-name)
  (setf *route-table* (make-instance 'route-table))
  (attach-routes-packet system-name))

(defun merge-hash-tables (&rest hash-tables)
  "Each subsequentional hash-table overrides existing key-values"
  (let ((final-hash-table (make-hash-table)))
    (loop for hash-table in hash-tables do
             (when hash-table
               (with-hash-table-iterator (next hash-table)
                 (loop
                   (multiple-value-bind (more? key value) (next)
                     (unless more?
                       (return))
                     (setf (gethash key final-hash-table) value))))))
    final-hash-table))

(defun query (method uri)
  (let ((*routes* (route-table-routes method)))
    (multiple-value-bind (route parameters) (try-match-url uri)
      (when route
        (let ((defaults (route-defaults route)))
          (setf parameters (merge-hash-tables defaults parameters)))
        (values route parameters)))))

;; example
#|
(define-routes :admin
  (route "/login" :methods :post :handler 'do-login)
  (route "/logout" :handler 'do-logout))

(define-routes (:my-app "")
  (include :admin)
  (route "/*path" :handler 'render-page)
  (route "/forum/:|topic-name|-:topic-id"))

|#
