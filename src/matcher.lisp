(in-package :http-routes.matcher)

(defmethod node-label-equal (label1 label2)
  nil)

(defmethod node-label-equal ((label1 symbol) (label2 symbol))
  (eq label1 label2))

(defmethod node-label-equal ((label1 character) (label2 character))
  (char-equal label1 label2))

(defmethod node-label-hash ((label symbol))
  (sb-impl::eql-hash label))

(defmethod node-label-hash ((label character))
  (char-code (char-upcase label)))

(sb-ext:define-hash-table-test node-label-equal node-label-hash)

(defstruct node
  (label)
  (name)
  (children (make-hash-table :test 'node-label-equal))
  (parent)
  (tag))

(defun result-pattern-length (source)
  (let ((length 0))
    (loop for source-part in source do
             (cond
               ((stringp source-part)
                (incf length (length source-part)))
               ((symbolp source-part)
                (incf length))
               ((listp source-part)
                (incf length (result-pattern-length source-part)))))
    length))

(defun copy-source-part-to-pattern (pattern pattern-index source-part)
  (cond
    ((symbolp source-part)
     (setf (aref pattern pattern-index) source-part)
     (incf pattern-index))
    ((stringp source-part)
     (loop for i from 0 below (length source-part) do
              (setf (aref pattern pattern-index)
                    (aref source-part i))
              (incf pattern-index))))
  pattern-index)

(defun create-pattern (source)
  (if (stringp source)
      source
      (if (= 1 (length source))
          (create-pattern (first source))
          (let ((pattern (make-array (result-pattern-length source)))
                (pattern-index 0))
            (loop for source-part in source do
                     (setf pattern-index (copy-source-part-to-pattern pattern pattern-index source-part)))
            pattern))))


(defun add-to-tree (tree pattern tag)
  (let ((parent tree))
    (loop for i from 0 below (length pattern) do
             (if-let ((node (gethash (if (symbolp (aref pattern i)) :wildcard (aref pattern i)) (node-children parent))))
               (setf parent node)
               (progn
                 (setf parent
                       (setf (gethash (if (symbolp (aref pattern i)) :wildcard (aref pattern i))
                                      (node-children parent))
                             (make-node :label (if (symbolp (aref pattern i)) :wildcard (aref pattern i))
                                        :name (aref pattern i)
                                        :parent parent))))))
    (setf (node-tag parent) tag)))

(defun children-count (node)
  (hash-table-count (node-children node)))

(defun children-find (node label)
  (gethash label (node-children node)))

(defun sub-wildcard-helper (wildcard wildcard-pos sequence sequence-index indexies)
  (if (= (children-count wildcard) 0)
      (progn
        (values :wildcard (node-tag wildcard) `((,(node-name wildcard) ,wildcard-pos))))
      ;; try match constant part after wildcard
      ;; moving window
      (loop for i from sequence-index to (length sequence) do
               (if (= i (length sequence))
                   (progn
                     (return (values :wildcard (node-tag wildcard) `((,(node-name wildcard) ,wildcard-pos)))))
                   (multiple-value-bind (match tag indexies%) (match% wildcard sequence i nil nil nil)
                     (if match
                         (return (values :wildcard tag (append `(,@indexies (,(node-name wildcard) ,wildcard-pos ,i) ,@indexies%))))))))))

(defun match% (node sequence sequence-index last-wildcard last-wildcard-pos indexies)
  (loop for i from sequence-index to (length sequence) do
           (if (= i (length sequence))
               (return (values t (node-tag node)))
               (progn
                 (when-let ((new-last-wildcard (children-find node :wildcard)))
                   (setf last-wildcard new-last-wildcard
                         last-wildcard-pos i))
                 (if-let ((child% (children-find node (aref sequence i))))
                   (setf node child%)
                   (if last-wildcard
                       (return (sub-wildcard-helper last-wildcard last-wildcard-pos sequence i indexies))
                       (return (values nil (node-tag node) i))))))))

(defun match (tree sequence)
  "returns two values FOUND and TAG"
  (match% tree sequence 0 nil 0 nil))

;; (defun down-from-star (star-node index string)
;;   (let ((cnode star-node))
;;     (loop for i from (1+ index) to (length string) do
;;              (if (= i (length string))
;;                  (return nil)
;;                  (multiple-value-bind (node end-index) (find-action cnode string i t)
;;                    (if node
;;                        (return (values node i))))))))

;; (defun up-to-the-star (cnode index)
;;   (declare (type node cnode)
;;            (type fixnum index)
;;            ;; (optimize (speed 3) (debug 0))
;;            )
;;   (let ((parent cnode))
;;     (loop for i from index downto 0 do
;;              (if-let ((node (gethash #\* (node-children parent))))
;;                (return (values node i))
;;                (setf parent (node-parent parent))))))

;; (defun find-action (tree string &optional (start 0) (exact nil))
;;   (declare (type node tree)
;;            (type string string)
;;            ;; (optimize (speed 3) (debug 0))
;;            )
;;   (let ((cnode tree))
;;     (loop for i from start to (length string) do
;;           (if (= i (length string))
;;               (progn
;;                 (or (and (node-action cnode)
;;                          (return (values cnode i)))
;;                     (and (not exact)
;;                          (multiple-value-bind (node index) (up-to-the-star cnode i)
;;                            (if node
;;                                (return (values node index))
;;                                (return nil))))))
;;                  (if-let ((node (gethash (aref string i) (node-children cnode))))
;;                    (setf cnode node)
;;                    (if-let ((star-node (gethash #\* (node-children cnode))))
;;                      (progn
;;                        (multiple-value-bind (node end-index) (down-from-star star-node i string)
;;                          (if node
;;                              (progn
;;                                (return (values node i end-index)))
;;                              (multiple-value-bind (node index) (up-to-the-star cnode i)
;;                                (if node
;;                                    (return (values node index))
;;                                    (return nil))))))
;;                      (multiple-value-bind (node index) (and (not exact) (up-to-the-star cnode i))
;;                        (if node
;;                            (return (values node index))
;;                            (return nil)))))))))

;; (defun maybe-remove-from-parent (node)
;;   ;; if it's single-node subtree
;;   (if-let ((parent (node-parent node)))
;;     (progn
;;       (node-char parent)
;;       (if (and (= 1 (hash-table-count (node-children parent))) (null (node-action parent)))
;;           (maybe-remove-from-parent parent)
;;           (remhash (node-char node) (node-children parent))))
;;     (setf (node-children node) (make-hash-table :test 'char-equal))))

;; (defun remove-action (tree string)
;;   (let ((parent tree))
;;     (loop for i from 0 below (length string) do
;;              (if-let ((node (gethash (aref string i) (node-children parent))))
;;                (progn (node-char node)
;;                       (setf parent node))
;;                (return (setf parent nil))))
;;     (when parent
;;       (setf (node-action parent) nil)
;;       (maybe-remove-from-parent parent))))
