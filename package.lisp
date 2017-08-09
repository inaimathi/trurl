;;;; package.lisp

(defpackage #:trurl
  (:use #:cl #:house #:cl-who #:cl-css #:parenscript)
  (:shadow #:%))

(in-package #:trurl)

(defparameter *update-frequency* 1)
