(in-package #:trurl)

(defparameter *grid* (lem:make-grid 60 30))

(defun reset! () (setf *grid* (lem:make-grid 60 30)))

(defun grid->json (grid &optional (from-x 0) (from-y 0) (to-x (lem:width grid)) (to-y (lem:height grid)))
  (->json
   (loop for y from from-y repeat to-y
      collect (loop for x from from-x repeat to-x
		 for c = (lem:occupant (lem:get-cell grid x y))
		 if (null c) collect nil
		 else collect (hash :state (lem::state c)
				    :type (lem:unit-type c))))))

(defun in-x-bound? (x) (>= (lem:width *grid*) x 0))
(defun in-y-bound? (y) (>= (lem:height *grid*) y 0))
