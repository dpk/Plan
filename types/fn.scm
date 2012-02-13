
(define-predicate p-fn?)
(define-operation (p-apply f args env k err))

(define (make-p-fn formals body env)
  (p-obj
    ((p-fn? f) #t)
    ((p-apply f actuals old-env k err)
      (cond ((p-compiled? body)
              (p-execute body (extend-env formals actuals env err) k err))
            ((procedure? body)
              (apply body (list actuals old-env k err)))
            (else
              (p-eval-each body (extend-env formals actuals env err) k err))))
    
    ((p= x y) (eq? x y))
    ((p-obj-type f) 'function)))

(define (extend-env formals actuals parent err)
  (make-p-env (env-zip (make-hash-table) formals actuals err) parent))

(define (env-zip hash keys values err) ; destructure args to function into hash
  (cond ((null? keys) (if (not (null? values))
                        (err 'incorrect-arity "wrong arity for procedure")))
        ((p-cons? keys)
          (if (p-cons? (p-car keys))
            (if (p-cons? (p-car values))
              (begin
                (env-zip hash (p-car keys) (p-car values) err)
                (env-zip hash (p-cdr keys) (p-cdr values) err))
              (err 'incorrect-arity "undestructurable argument given where destructurable argument expected"))
            (begin
              (hash-table-set! hash (p-car keys) (p-car values))
              (env-zip hash (p-cdr keys) (p-cdr values) err))))
        (else (hash-table-set! hash keys values))) hash)
