(in-package #:trurl)

;;;;;;;;;; Initial state setup
(lem:seed! *grid* 10 10 (wander))
(lem:seed! *grid* 15 15 (wander))
(lem:seed! *grid* 30 17 (lem:ray))
(lem:seed! *grid* 20 20 (lem:line))
(lem:seed! *grid* 50 20 (lem:box))

(bt:make-thread
 (lambda ()
   (loop do (lem:step-grid! *grid*)
      do (sleep *update-frequency*))))
