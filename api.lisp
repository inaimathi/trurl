(in-package #:trurl)

(define-handler (/api/look-at :content-type "application/javascript") ((from-x :integer (in-x-bound? from-x))
								       (from-y :integer (in-y-bound? from-y))
								       (to-x :integer (in-x-bound? to-x))
								       (to-y :integer (in-x-bound? to-y)))
  (grid->json *grid* from-x from-y to-x to-y))
