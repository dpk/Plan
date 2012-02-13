
(define-predicate p-env?)
(define-operation (p-env-lookup env sym))
(define-operation (p-env-set env sym val))
(define-operation (p-env-set! env sym val))
(define-operation (p-toplevel-env env))

(define no-binding* (gensym))

(define (make-p-env bindings parent-env)
  (p-obj
    ((p-env? env) #t)
    ((p-env-lookup env sym)
      (cond ((hash-table-exists? bindings sym)
              (hash-table-ref bindings sym))
            ((not (null? parent-env))
              (p-env-lookup parent-env sym))
            (else no-binding*)))
    ((p-toplevel-env env)
      (if (null? parent-env) env
            (p-toplevel-env parent-env)))
    ((p-env-set env sym val)
      (cond ((or (hash-table-exists? bindings sym) (null? parent-env))
              (hash-table-set! bindings sym val))
            (else (p-env-set parent-env sym val))))
    ((p-env-set! env sym val)
      (hash-table-set! (tags val) 'name sym)
      (p-env-set (p-toplevel-env env) sym val))
    
    ((p= x y) (eq? x y))
    ((p-obj-type x) 'binding)))
