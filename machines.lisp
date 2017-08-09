(in-package #:trurl)

(lem:define-machine wander
  (let ((dest (pick (loop for (x y) in lem:n*extended
		       for c = (lem:neighbor x y)
		       when (and c (lem:empty? c)) collect c))))
    (when dest (lem:move-to! dest lem:here))))
