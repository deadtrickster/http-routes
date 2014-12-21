(in-package :http-routes.routes)

;;adopted from fast-http
;;#####
(defmacro casev (keyform &body clauses)
  (alexandria:once-only (keyform)
    (flet ((get-val (val)
             (cond
               ((eq val 'otherwise) val)
               ((symbolp val) (symbol-value val))
               ((constantp val) val)
               (T (error "CASEV can be used only with variables or constants")))))
      `(ecase ,keyform
         ,@(loop for (val . clause) in clauses
                 if (eq val 'otherwise)
                 collect `(otherwise ,@clause)
                 else if (listp val)
                 collect `((,@(mapcar #'get-val val)) ,@clause)
                 else
                 collect `(,(get-val val) ,@clause))))))

(defmacro tagcasev (keyform &body blocks)
  (let ((end (gensym "END")))
    `(tagbody
        (casev ,keyform
          ,@(loop for (tag . body) in blocks
                  if (eq tag 'otherwise)
                  collect `(otherwise ,@body (go ,end))
                  else
                  collect `(,tag (go ,(if (listp tag) (car tag) tag)))))
        (go ,end)
        ,@(loop for (tag . body) in blocks
                if (listp tag)
                append tag
                else if (not (eq tag 'otherwise))
                collect tag
                collect `(progn ,@body
                                (go ,end)))
        ,end)))

(defmacro go-state (state &key (advance t) (set-state t))
  `(locally (declare (optimize (speed 3) (safety 0)))
     ,@(and set-state
            `((setf state ,state)))
     ,(if advance
          '(go :iteration-exit)
          `(go ,state))))
;;#####


(defmacro while (condition &body body)
  `(loop (if (not ,condition)
             (return)
             (progn
               ,@body))))

(defmacro dispatch-cchar (&key (advance t))
  `(case cchar
     (#\(
      (go-state :optional-start :advance ,advance))
     (#\)
      (go-state :optional-end :advance ,advance))
     (#\:
      (go-state :param-name-start :advance ,advance))
     (#\*
      (go-state :rest-name-start :advance ,advance))
     (#\\
      (go-state :escape :advance ,advance))
     (:eof
      (go-state :eof :advance nil))
     (t
      (go-state :literal :advance nil))))

(defmacro current-char ()
  `(if (= i (length string)) :eof (aref string i)))

(defun literal-character-p (char)
  (case char
    (#\(
     nil)
    (#\)
     nil)
    (#\:
     nil)
    (#\*
     nil)
    (#\\
     nil)
    (:eof
     nil)
    (t
     t)))

(defun parse-route (string)
  (collectors:with-collector-output (add-route-part)
    (let ((state :literal)
          (current-literal-buffer)
          (current-parameter-buffer)
          (current-optional-buffer)
          (param-name-fenced))
      (loop for i from 0 to (length string)
            as cchar = (current-char) do
               (if (eq cchar :eof)
                   (setf state :eof))
               (tagbody
                  (tagcasev state
                    (:optional-start
                     (go-state :optional-collector))
                    (:optional-collector
                     (when (eql #\) cchar)
                       (go-state :optional-end :Advance nil))
                     (unless current-optional-buffer
                       (setf current-optional-buffer (collectors:make-appender)))
                     (funcall current-optional-buffer cchar))
                    (:optional-end
                     (let ((char-list (funcall current-optional-buffer)))
                       (add-route-part `(&optional ,@(parse-route (coerce char-list 'string)))))
                     (setf current-optional-buffer nil)
                     (go-state :literal-start))
                    (:param-name-start
                     (go-state :param-name))
                    (:param-name
                     (if (or (not (literal-character-p cchar)) (eql #\/ cchar) (eql #\) cchar))
                         (go-state :param-name-end :advance nil))
                     (if (and param-name-fenced (eql #\| cchar))
                         (go-state :param-name-end :advance t))
                     (unless current-parameter-buffer
                       (setf current-parameter-buffer (collectors:make-appender))
                       (if (eql #\| cchar)
                           (setf param-name-fenced t)))
                     (unless (eql #\| cchar)
                       (funcall current-parameter-buffer cchar)))
                    (:param-name-end
                     (when current-parameter-buffer
                       (let ((char-list (funcall current-parameter-buffer)))
                         (add-route-part (alexandria:make-keyword (string-upcase (coerce char-list 'string)))))
                       (setf current-parameter-buffer nil)
                       (setf param-name-fenced nil)
                       (dispatch-cchar :advance nil)))
                    (:rest-name-start
                     (go-state :rest-name))
                    (:rest-name
                     (if (eql #\/ cchar)
                         (go-state :param-name-end :advance nil))
                     (if (and param-name-fenced (eql #\| cchar))
                         (go-state :param-name-end :advance t))
                     (unless current-parameter-buffer
                       (setf current-parameter-buffer (collectors:make-appender))
                       (add-route-part '&rest)
                       (if (eql #\| cchar)
                           (setf param-name-fenced t)))
                     (unless (eql #\| cchar)
                       (funcall current-parameter-buffer cchar)))
                    (:literal-start
                     (if (literal-character-p cchar)
                         (go-state :literal :advance nil)
                         (go-state :literal-end :advance nil)))
                    (:literal
                     (unless current-literal-buffer
                       (setf current-literal-buffer (collectors:make-appender)))
                     (funcall current-literal-buffer cchar)
                     (go-state :literal-start))
                    (:literal-end
                     (when current-literal-buffer
                       (let ((char-list (funcall current-literal-buffer)))
                         (add-route-part (coerce char-list 'string)))
                       (setf current-literal-buffer nil))
                     (dispatch-cchar :advance nil))
                    (:escape
                     (go-state :literal))
                    (:eof
                     (when current-literal-buffer
                       (let ((char-list (funcall current-literal-buffer)))
                         (add-route-part (coerce char-list 'string)))
                       (setf current-literal-buffer nil))
                     (when current-parameter-buffer
                       (let ((char-list (funcall current-parameter-buffer)))
                         (add-route-part (alexandria:make-keyword (string-upcase (coerce char-list 'string)))))
                       (setf current-parameter-buffer nil))
                     (when current-optional-buffer
                       (let ((char-list (funcall current-optional-buffer)))
                         (add-route-part `(&optional ,@(parse-route (coerce char-list 'string))))))
                     (return)))
                :iteration-exit)))))

(defun route-variables (route)
  (collectors:with-collector-output (add-variable)
    (let ((next-is-rest))
      (loop for route-part in route do
               (cond
                 ((stringp route-part))
                 ((eql route-part '&rest)
                  (setf next-is-rest t))
                 ((symbolp route-part)
                  (if next-is-rest
                      (progn
                        (add-variable `(:multi-segment ,route-part))
                        (setf next-is-rest nil))
                      (progn
                        (add-variable `(:segment ,route-part)))))
                 ((and
                   (listp route-part)
                   (eq (first route-part) '&optional))
                  (if next-is-rest
                      (error "Invalid part after multi-segment variable specificator")
                      (let ((optional-part-var (route-variables (rest route-part))))
                        (when optional-part-var
                          (add-variable (first optional-part-var)))))))))))

(defun route-to-match-rules (route)
  (let ((rules))
    (let ((rule-parts nil))
      (flet ((add-rule (rule)
               (setf rules (append rules (list rule))))
             (add-rule-part (part &optional (required t))
               (setf rule-parts (append rule-parts (if (listp part) part (list part))))
               (when (and required rules)
                 (let ((new-rules))
                   (loop for rule in rules do
                            (setf new-rules (if new-rules
                                                (append new-rules (append rule (list part)))
                                                (list (append rule (list part))))))
                   (setf rules new-rules)))))
        (loop for route-part in route do
                 (cond
                   ((stringp route-part)
                    (add-rule-part route-part))
                   ((and (symbolp route-part)
                         (not (eq '&rest route-part))
                         (not (eql '&optional route-part)))
                    (add-rule-part :wildcard))
                   ((and
                     (listp route-part)
                     (eq (first route-part) '&optional))
                    (add-rule rule-parts)
                    (let ((optional-part (route-to-match-rules (rest route-part))))
                      (add-rule-part (first optional-part) nil)))))
        (add-rule rule-parts)
        rules))))
