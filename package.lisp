;;;; package.lisp

(defpackage #:trurl
  (:use #:cl #:house #:cl-who #:cl-css #:parenscript)
  (:import-from #:lem #:ray #:line #:box)
  (:shadow #:% #:start)
  (:export :start))

(in-package #:trurl)

(defparameter *update-frequency* 1)

(define-http-type (:atom)
    :type-expression `(let ((name (intern (string-upcase ,parameter) :trurl)))
			(and (find-class name) (fdefinition name)
			     (funcall (fdefinition name))))
    :type-assertion `(typep ,parameter 'lem:unit))
