;;;; trurl.asd

(asdf:defsystem #:trurl
  :description "Describe trurl here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:house
               #:cl-who
	       #:yason
               #:parenscript
               #:cl-css
               #:lem
	       #:bordeaux-threads)
  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "machines")
	       (:file "css") (:file "front-end") (:file "api")
               (:file "trurl")))
