(in-package :http-routes.routes)

(defvar *routes*)

(defun create-params-wrapper (variables tag)
  (compile nil `(lambda (url matches)
                  (declare (ignorable url matches))
                  (block :validation
                    (let ((parameters (make-hash-table)))
                      ,(if variables
                           `(let ((validp t))
                              (loop  for match in matches do
                                       (let ((matched-str (cond
                                                            ((= (length match) 2)
                                                             (subseq url (second match)))
                                                            ((= (length match) 3)
                                                             (subseq url (second match) (third match))))))
                                         (destructuring-bind (name . type) (assoc (first match) ',variables)
                                           (case type
                                             (:multi-segment
                                              (setf (gethash name parameters) matched-str))
                                             (:segment
                                              (when (and matched-str
                                                         (find #\/ matched-str))
                                                (setf validp nil)
                                                (return))
                                              (setf (gethash name parameters) matched-str))))))
                              (unless validp
                                (return-from :validation (values nil parameters)))))
                      (values ,tag parameters))))))

(defun add-route (route tag)
  (let* ((parsed-route (if (stringp route) (parse-route route) route))
         (variables (route-variables parsed-route))
         (match-rules (route-to-match-rules parsed-route))
         (tag% (create-params-wrapper variables tag)))
    (loop for match-rule in match-rules
          as match-pattern = (create-pattern match-rule) do
             (add-to-tree *routes* match-pattern tag%))))

(defun make-routes ()
  (make-node))

(defun try-match-url (url)
  (multiple-value-bind (matched tag index) (match *routes* url)
    (if (and matched tag)
        (funcall tag url index))))
