
(in-package :http-routes.helpers)

(defun nth-segment (path index)
  (elt (split-sequence:split-sequence #\/ path :remove-empty-subseqs t :count (1+ index)) index))

(defun to-integer (value &key junk-allowed)
  (handler-case
      (parse-integer value :junk-allowed junk-allowed)
    (error () nil)))

(defun to-number (value)
  (handler-case
      (parse-number:parse-number value)
    (error () nil)))
