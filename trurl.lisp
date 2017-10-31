(in-package #:trurl)

(bt:make-thread
 (lambda ()
   (loop do (ignore-errors (lem:step! *grid*))
      do (sleep *update-frequency*))))

(defun start (port)
  (house:start port))
