(in-package #:trurl)

(bt:make-thread
 (lambda ()
   (loop do (lem:step! *grid*)
      do (sleep *update-frequency*))))

(defun start (port)
  (house:start port))
