(in-package :http-routes)

(defvar *route-table*)

(defvar *route-section* "")

(defclass route-table ()
  ((methods :initform (alexandria:alist-hash-table
                       `((:get . ,(http-routes.matcher::make-node))
                         (:head . ,(http-routes.matcher::make-node))
                         (:post . ,(http-routes.matcher::make-node))
                         (:put . ,(http-routes.matcher::make-node))
                         (:delete . ,(http-routes.matcher::make-node))
                         (:trace . ,(http-routes.matcher::make-node))
                         (:options . ,(http-routes.matcher::make-node))
                         (:connect . ,(http-routes.matcher::make-node))
                         (:patch . ,(http-routes.matcher::make-node)))))
   (named-routes :initform (make-hash-table))))

(defun make-route-table (&rest methods)
  (let ((route-table (make-instance 'route-table)))
    (loop for method in methods do
          (setf (gethash method (slot-value route-table 'methods)) (http-routes.matcher::make-node)))
    route-table))

(defun named-routes ()
  (slot-value *route-table* 'named-routes))

(defun get-named-route (name)
  (gethash name (named-routes)))

(defun add-named-route (name route)
  (if (get-named-route name)
      (error "Route with name ~a already attached" name)
      (setf (gethash name (named-routes)) route)))

(defclass route ()
  ((methods :reader route-method :initarg :methods)
   (path :reader route-path :initarg :path)
   (handler :reader route-handler :initarg :handler)
   (defaults :reader route-defaults :initarg :defaults)
   (validators :reader route-validators :initarg :validators)
   (path-generator :reader route-path-generator :initarg :path-generator)))

(defun path-for (name-or-route &optional args)
  (let ((route (if (typep name-or-route 'route) name-or-route (get-named-route name-or-route))))
    (unless route
      (error "Unable to find named route ~a in current route table" name-or-route))
    (funcall (route-path-generator route) args)))

(defmethod attach-routes-packet (system &key section)
  (declare (ignore system section))
  nil)

(defmacro define-routes (name &body routes)
  (destructuring-bind (name section) (if (listp name)
                                         name
                                         (list name (concatenate 'string "/" (string-downcase (symbol-name name)))))
    (let ((include-name (intern "INCLUDE" *package*)))
      `(macrolet ((,include-name (packet-to-include &key (section nil section-supplied-p))
                    (if section-supplied-p
                        `(attach-routes-packet ,packet-to-include :section ,section)
                        `(attach-routes-packet ,packet-to-include))))
         (defmethod attach-routes-packet ((system (eql ,name)) &key (section ,section))
           (let ((*route-section* section))
             ,@routes))))))

(defun route-table-routes (method)
  (gethash method (slot-value *route-table* 'methods)))

(defun add-route-to-routes (routes path route)
  (let ((*routes* routes))
    (http-routes.routes:add-route path route)))

(defun add-route% (path handler methods defaults validators path-generator)
  (unless methods
    (setf methods '(:get)))
  (let ((route (make-instance 'route
                              :methods methods
                              :handler handler
                              :path path
                              :defaults defaults
                              :validators validators
                              :path-generator path-generator)))
    (loop for method in methods
          as routes = (route-table-routes method) do
             (add-route-to-routes routes path route))
    route))

(defun create-path-generator (fname route)
  (let* ((parsed-route (http-routes.parser:parse-route route))
         (variables (http-routes.parser:route-variables parsed-route))
         (rules (http-routes.parser:route-to-match-rules parsed-route)))
    (compile nil `(lambda (vars)
                    (flet ((find-rule-for-vars (vars)
                             (loop for rule in ',rules do
                                      (if (= (length vars) (count-if (lambda (item) (symbolp item)) rule))
                                          (return rule))))
                           (validate-vars (vars)
                             (loop for (name . values) in vars do
                                      (unless (assoc name ',variables)
                                        (error "Unknown variable ~a for route ~a" name ',fname)))))
                      (validate-vars vars)
                      (let ((rule (find-rule-for-vars vars)))
                        (unless rule
                          (error "Unable to find rule"))
                        (with-output-to-string (stream)
                          (loop for item in rule do
                                   (if (symbolp item)
                                       (alexandria:if-let ((value (alexandria:assoc-value vars item)))
                                         (princ value stream)
                                         (error "unable to find value for ~a" item))
                                       (princ item stream))))))))))

(defmacro route (uri &key handler methods name defaults validators)
  `(let* ((path (concatenate 'string *route-section* ,uri))
          (path-generator (if ,name (create-path-generator ,name path))))
     (let ((route (add-route% path ,handler ,methods  ,defaults ,validators (if ,name path-generator nil))))
       (if ,name
           (add-named-route ,name route))
       route)))

(defun get (uri &key handler name defaults validators)
  (route uri :handler handler :methods '(:get) :name name :defaults defaults :validators validators))

(defun head (uri &key handler name defaults validators)
  (route uri :handler handler :methods '(:head) :name name :defaults defaults :validators validators))

(defun post (uri &key handler name defaults validators)
  (route uri :handler handler :methods '(:post) :name name :defaults defaults :validators validators))

(defun put (uri &key handler name defaults validators)
  (route uri :handler handler :methods '(:put) :name name :defaults defaults :validators validators))

(defun delete (uri &key handler name defaults validators)
  (route uri :handler handler :methods '(:delete) :name name :defaults defaults :validators validators))

(defun trace (uri &key handler name defaults validators)
  (route uri :handler handler :methods '(:trace) :name name :defaults defaults :validators validators))

(defun options (uri &key handler name defaults validators)
  (route uri :handler handler :methods '(:options) :name name :defaults defaults :validators validators))

(defun connect (uri &key handler name defaults validators)
  (route uri :handler handler :methods '(:connect) :name name :defaults defaults :validators validators))

(defun patch (uri &key handler name defaults validators)
  (route uri :handler handler :methods '(:patch) :name name :defaults defaults :validators validators))


(defmacro root (&key handler name)
  `(route "/" :to ,handler :name ,name))

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
