
(define-predicate p-continuation?)
(define-operation (p-invoke conti val))

(define (p-reify-conti k)
  (p-obj
    ((p-invoke conti val) (k val))
    
    ((p-continuation? conti) #t)
    ((p= x y) (eq? x y))
    ((p-obj-type x) 'continuation)))
