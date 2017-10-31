(in-package #:trurl)

(let ((s *standard-output*))
  (defun log! (label &rest args)
    (format s "~s: ~{~s~^ ~}~%" label args)))

(defun hash (&rest k/v-pairs)
  (let ((h (make-hash-table)))
    (loop for (k v) on k/v-pairs by #'cddr
       do (setf (gethash k h) v))
    h))

(defmethod pick ((arr array))
  (when (not (zerop (length arr))) (aref arr (random (length arr)))))
(defmethod pick ((lst list))
  (when lst (nth (random (length lst)) lst)))

(defun shuffle! (lst) (sort lst #'< :key (lambda (v) (declare (ignore v)) (random 1.0))))

(defmethod yason:encode ((object symbol) &optional stream)
  (yason:encode (string-downcase (symbol-name object)) stream))

(defun ->json (thing)
  (with-output-to-string (s)
    (yason:encode thing s)))
