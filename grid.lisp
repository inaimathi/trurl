(in-package #:trurl)

(defparameter *grid* (lem:make-grid 65 35))

(defun grid->json (grid &optional (from-x 0) (from-y 0) (to-x (lem:grid-width grid)) (to-y (lem:grid-height grid)))
  (with-output-to-string (s)
    (yason:encode
     (loop for y from from-y to (- to-y 1)
	collect (loop for x from from-x to (- to-x 1)
		   for c = (lem:occupant (lem:get-cell grid x y))
		   if (null c) collect nil
		   else collect (hash :state (lem::state c)
				      :type (lem:unit-type c))))
     s)))

(defun in-x-bound? (foo) (>= (lem:grid-width *grid*) foo 0))
(defun in-y-bound? (foo) (>= (lem:grid-height *grid*) foo 0))
