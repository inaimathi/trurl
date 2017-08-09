(in-package #:trurl)

(define-handler (/api/inventory :content-type "application/json") ()
  (->json *machines*))

(define-handler (/api/look-at :content-type "application/json") ((from-x :integer (in-x-bound? from-x))
								 (from-y :integer (in-y-bound? from-y))
								 (to-x :integer (in-x-bound? to-x))
								 (to-y :integer (in-x-bound? to-y)))
  (grid->json *grid* from-x from-y to-x to-y))

(define-handler (/api/place :content-type "application/json" :method :post) ((x :integer (in-x-bound? x)) (y :integer (in-x-bound? y)) (atom :atom))
  (lem:seed! *grid* x y atom)
  (->json "ok"))
