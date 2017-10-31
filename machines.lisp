(in-package #:trurl)

(defparameter *machines*
  '(line ray box wander bomb ff gg bunny router))

(lem:define-machine wander
  (let ((dest (pick (loop for (x y) in lem:n*extended
		       for c = (lem:neighbor x y)
		       when (and c (lem:empty? c)) collect c))))
    (when dest (lem:move-to! dest lem:here))))

(lem:define-machine bunny
  (let* ((empties (loop for (x y) in lem:n*extended
		     for c = (lem:neighbor x y)
		     when (and c (lem:empty? c)) collect c))
	 (dest (pick empties))
	 (child (pick empties)))
    (when (and child (> (length empties) 35))
      (lem:spawn-in! child lem:self))
    (when (and dest (not (equal dest child)))
      (lem:move-to! dest lem:here))))

(lem:define-machine lemon nil)

(lem:define-machine router-endpoint nil)

(lem:define-machine router-fluid
  (let ((w (lem:get-state lem:self :w 10))
	(h (lem:get-state lem:self :h 10))
	(x (lem:get-state lem:self :x 0))
	(y (lem:get-state lem:self :y 0))
	(ends (loop for (x y) in lem:n*extended
		 for c = (lem:neighbor x y)
		 when (and c (typep (lem:occupant c) 'router-endpoint)) collect c)))
    (labels ((any-of (cell &rest types)
	       (let ((o (lem:occupant cell)))
		 (some (lambda (tp) (typep o tp)) types)))
	     (spawn! (neigh &key (x x) (y y))
	       (when neigh
		 (unless (any-of neigh 'router-endpoint 'router-fluid)
		   (lem:spawn-in! neigh lem:self :x x :y y :h h :w w)))))
      (when (null ends) (lem:spawn-in! lem:here (router-endpoint)))
      (when (> w x) (spawn! (lem:neighbor -1 0) :x (+ x 1)))
      (when (> h y) (spawn! (lem:neighbor 0 -1) :y (+ y 1)))
      (unless (zerop x) (spawn! (lem:neighbor 1 0) :x (- x 1)))
      (unless (zerop y) (spawn! (lem:neighbor 0 1) :y (- y 1))))))

(lem:define-machine router
  (flet ((neighbors-at (fn)
	   (loop for (x y) in lem:n*extended
	      for c = (lem:neighbor x y)
	      when (and c (funcall fn x y)) collect c)))
    (let* ((w 25) (h 25)
	   (x (lem:get-state lem:self :x 0))
	   (y (lem:get-state lem:self :y 0))
	   (outsides (concatenate
		      'list
		      (when (zerop x) (neighbors-at (lambda (x y) (> x 0))))
		      (when (zerop y) (neighbors-at (lambda (x y) (> y 0))))
		      (when (= x w) (neighbors-at (lambda (x y) (> 0 x))))
		      (when (= y h) (neighbors-at (lambda (x y) (> 0 y)))))))
      (when (and (zerop y) (zerop x))
	(lem:spawn-in! (lem:neighbor -1 -1) (router-fluid) :h (- h 2) :w (- w 2))
	(lem:spawn-in! (lem:neighbor -2 -2) (router-endpoint)))
      (loop for c in outsides do (lem:spawn-in! c (lemon)))

      (when (and (> w x) (or (= y 0) (= y h)))
	(lem:spawn-in! (lem:neighbor -1 0) lem:self :x (+ x 1) :y y))
      (unless (= 0 x)
	(when (or (= y 0) (= y h))
	  (lem:spawn-in! (lem:neighbor 1 0) lem:self :x (- x 1) :y y)))
      (when (and (> h y) (or (= x 0) (= x w)))
	(lem:spawn-in! (lem:neighbor 0 -1) lem:self :x x :y (+ y 1)))
      (unless (= 0 y)
	(when (or (= x 0) (= x w))
	  (lem:spawn-in! (lem:neighbor 0 1) lem:self :x x :y (- y 1)))))))

;; (lem:define-machine builder
;;   (flet ((all-empty? ()
;; 	   (every
;; 	    (lambda (xy)
;; 	      (if (equal '(0 0) xy)
;; 		  t
;; 		  (lem:empty? (lem:neighbor (first xy) (second xy)))))
;; 	    lem:n*extended)))
;;     (let ((dest (pick (loop for (x y) in lem:n*extended
;; 			 for c = (lem:neighbor x y)
;; 			 when (and c (lem:empty? c)) collect c)))))))

(lem:define-machine bomb
  (loop for (x y) in lem:n*extended
     do (lem:empty! (lem:neighbor x y))))

(lem:define-machine ff
  (loop for (x y) in lem:n*extended
     for c = (lem:neighbor x y)
     do (when (lem:empty? c) (lem:spawn-in! c lem:self))))

(lem:define-machine gg
    (loop for (x y) in lem:n*extended
	 do (lem:spawn-in! (lem:neighbor x y) lem:self)))
