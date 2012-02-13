
(define-syntax p-comprim
  (er-macro-transformer (lambda (exp rename compare)
    (let ((name (caadr exp))
          (formals (cdadr exp))
          (body (cddr exp)))
      `(hash-table-set! prim-compilers* (p-env-lookup toplevel-env ',name)
        (lambda (args senv)
          (apply (lambda ,formals ,@body) args)))))))

(define-predicate p-compiled?)
(define-operation (p-execute compiled env k err))
(define prim-compilers* (make-hash-table))

(define (compile exp senv) ; senv = static environment
  (display (string-append "compiling " (p-literal exp) "\n"))
  (cond ((p-cons? exp)
          (let ((operator (p-car exp)))
            (if (symbol? operator)
                  (let ((actualop (p-env-lookup senv operator)))
                    (cond ((p-macro? actualop)
                            (p-expand actualop (p-cdr exp) senv (lambda (expansion) (compile expansion senv)) top-error-handler))
                          ((and (p-prim? actualop) (hash-table-exists? prim-compilers* actualop))
                            (make-compiled (p-prim-compile (hash-table-ref prim-compilers* actualop) (p-cdr exp) senv)))
                          (else
                            (compile-to-eval exp))))
                  (compile-to-eval exp))))
        ((symbol? exp)
          (make-compiled (lambda (env k err)
            (p-env-lookup env exp))))
        (else (compile-to-eval exp))))

(define (compile-to-eval exp)
  (display (string-append "giving up, compiling to eval: " (p-literal exp) "\n"))
  (make-compiled
    (lambda (env k err)
      (p-eval exp env k err))))

(define (make-compiled proc)
  (p-obj
    ((p-execute exp env k err)
      (proc env k err))
    ((p-compiled? exp) #t)
    ((p-obj-type exp) 'compiled)))

(define (p-prim-compile compiler actuals senv)
  (compiler (plan->scheme-list actuals) senv))
(define (p-compile-seq seq senv)
  (make-compiled
    (if (null? (p-cdr seq))
          (let ((compiled (compile (p-car seq) senv)))
            (lambda (env k err)
              (p-execute compiled env k err)))
          (let ((compiled1 (compile (p-car seq) senv))
                (compiled-rest (p-compile-seq (p-cdr seq) senv)))
            (lambda (env k err)
              (p-execute compiled1 env (lambda (res) (p-execute compiled-rest env k err)) err))))))
