
(define-predicate p-prim?)

(define (make-p-prim arglen scheme-proc)
  (p-obj
    ((p-prim? x) #t)
    ((p-apply f actuals env k err)
      (apply scheme-proc (list actuals env k err)))
    ((p= x y) (eq? x y))
    ((p-obj-type x) 'primitive)))
