
(define-predicate p-macro?)
(define-operation (p-expand m args env k err))

(define (make-p-macro formals body env)
  (join (p-obj
    ((p-fn? m) #f)
    ((p-macro? m) #t)
    ((p-obj-type m) 'macro)
    ((p-expand m args env k err) (p-apply m args env k err)))
   (make-p-fn formals body env)))
