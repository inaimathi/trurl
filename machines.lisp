(in-package #:trurl)

(defparameter *machines*
  '(line ray box wander bomb ff))

(lem:define-machine wander
  (let ((dest (pick (loop for (x y) in lem:n*extended
		       for c = (lem:neighbor x y)
		       when (and c (lem:empty? c)) collect c))))
    (when dest (lem:move-to! dest lem:here))))

(lem:define-machine bomb
  (loop for (x y) in lem:n*extended
     do (lem:empty! (lem:neighbor x y))))

(lem:define-machine ff
  (loop for (x y) in lem:n*extended
     for c = (lem:neighbor x y)
     do (when (lem:empty? c) (lem:spawn-in! c lem:self))))
