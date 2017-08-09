(in-package #:trurl)

(defun hash (&rest k/v-pairs)
  (let ((h (make-hash-table)))
    (loop for (k v) on k/v-pairs by #'cddr
       do (setf (gethash k h) v))
    h))

(defmethod pick ((arr array)) (aref arr (random (length arr))))
(defmethod pick ((lst list)) (nth (random (length lst)) lst))

(defmethod yason:encode ((object symbol) &optional stream)
  (yason:encode (string-downcase (symbol-name object)) stream))

(defun ->json (thing)
  (with-output-to-string (s)
    (yason:encode thing s)))
