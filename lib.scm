
(p-deffn (cons the-car the-cdr)
  (k (p-cons the-car the-cdr)))

(p-defprim (quote x) (k x))
(p-comprim (quote x)
  (lambda (env k err)
    (k x)))

(p-defprim (fn formals . body)
  (k (make-p-fn (scheme->plan-list formals) (scheme->plan-list body) env)))
(p-comprim (fn formals . body)
  (let ((compiled-body (p-compile-seq (scheme->plan-list body) senv)))
    (lambda (env k err)
      (k (make-p-fn (scheme->plan-list formals) compiled-body env)))))

(p-defprim (mac formals . body)
  (k (make-p-macro (scheme->plan-list formals) (scheme->plan-list body) env)))
(p-comprim (mac formals . body)
  (let ((compiled-body (p-compile-seq (scheme->plan-list body) senv)))
    (lambda (env k err)
      (k (make-p-macro (scheme->plan-list formals) compiled-body env)))))

(define (rewrite-if clauses)
  (let ((ncl (length clauses)))
    (cond ((= ncl 0) `(() () ()))
          ((= ncl 1) `(,(car clauses) #t ()))
          ((= ncl 2) `(,(car clauses) ,(cadr clauses) ()))
          ((= ncl 3) clauses)
          ((> ncl 3) `(,(car clauses) ,(cadr clauses) ,(p-cons 'if (scheme->plan-list (rewrite-if (cddr clauses)))))))))
(p-defprim (if . clauses)
  (p-eval (car clauses) env (lambda (pred)
    (if (not (null? pred))
          (if (null? (cdr clauses)) (k #t)
               (p-eval (cadr clauses) env k err))
          (cond ((null? (cddr clauses)) (k '()))
                ((null? (cdddr clauses)) (p-eval (caddr clauses) env k err))
                (else (p-eval (scheme->plan-list (cons 'if (cddr clauses))) env k err))))) err))
(p-comprim (if . clauses)
  (let* ((newcl  (rewrite-if clauses))
         (pred   (compile (car newcl) senv))
         (conseq (compile (cadr newcl) senv))
         (alt    (compile (caddr newcl) senv)))
    (lambda (env k err)
      (p-execute pred env (lambda (predval)
        (if (not (null? predval))
              (p-execute conseq env k err)
              (p-execute alt env k err))) err))))

(p-deffn (car xs) (k (p-car xs)))
(p-deffn (cdr xs) (k (p-cdr xs)))

(p-defprim (set var val) ; todo: add setter transformation
  (if (p-cons? var)
        (p-eval (scheme->plan-list `(,(p-cons 'setter (p-cons (p-car val) '())) ,@(plan->scheme-list (p-cdr val)) ,val)) env k err)
        (p-eval val env (lambda (evl)
          (p-env-set env var evl) (k evl)) err)))
(p-comprim (set var val)
  (let ((cval (compile val senv)))
    (lambda (env k err)
      (p-execute cval env (lambda (valex) (p-env-set env var valex) (k valex)) err))))
(p-defprim (set! var val)
  (p-eval val env (lambda (evl)
    (p-env-set! env var evl) (k evl)) err))
(p-comprim (set! var val)
  (let ((cval (compile val senv)))
    (lambda (env k err)
      (p-execute cval env (lambda (valex) (p-env-set! env var valex) (k valex)) err))))

(p-deffn (apply f args)
  (p-apply f args env k err))
(p-deffn (eval eexp . eenv)
  (if (pair? eenv)
    (p-eval eexp (car eenv) k err)
    (p-eval eexp env k err)))

(p-deffn (+ . xs)
  (k (apply + xs)))
(p-deffn (- . xs)
  (k (apply - xs)))
(p-deffn (* . xs)
  (k (apply * xs)))
(p-deffn (/ . xs)
  (k (apply / xs)))

(p-deffn (numerator n)
  (k (numerator n)))
(p-deffn (denominator n)
  (k (denominator n)))

(p-deffn (binding) (k env))

(p-defprim (unquote x) (err 'malformed-quasiquote "can't have unquote (,) outside of quasiquote"))
(p-defprim (unquote-splicing x) (err 'malformed-quasiquote "can't have unquote-splicing (,@) outside of quasiquote"))

(p-defprim (quasiquote x)
  (if (not (p-cons? x))
        (k x)
        (cond ((eq? 'unquote (p-car x))
                (p-eval (p-car (p-cdr (p-car x))) env k err))
              ((eq? 'unquote-splicing (p-car x))
                (err 'malformed-quasiquote "can't have unquote-splicing (,@) in top position of quasiquote"))
              (else (p-do-qq x env k err)))))
(define (p-append . xss)
  (scheme->plan-list (apply append (map plan->scheme-list xss))))
(define (p-append-k x k)
  (lambda (y)
    (k (p-append x y))))
(define (p-do-qq x env k err)
  (cond ((not (p-cons? x)) (k x))
        ((p-cons? (p-car x))
          (let ((qcomb (p-car x)) (qopr (p-car (p-car x))) (qopd (p-car (p-cdr (p-car x)))))
            (cond ((eq? qopr 'unquote)
                    (p-eval qopd env (lambda (eqopd)
                      (p-do-qq (p-cdr x) env (p-cons-k eqopd k) err)) err))
                  ((eq? qopr 'unquote-splicing)
                    (p-eval qopd env (lambda (eqopd)
                      (p-do-qq (p-cdr x) env (p-append-k eqopd k) err)) err))
                  (else
                    (p-do-qq (p-car x) env (lambda (qqcar)
                      (p-do-qq (p-cdr x) env (p-cons-k qqcar k) err)) err)))))
          (else (p-do-qq (p-cdr x) env (p-cons-k (p-car x) k) err))))

(p-deffn (macro-expand unexp)
  (p-eval (p-car unexp) env (lambda (mac)
    (p-expand mac (p-cdr unexp) env k err)) err))

(p-deffn (call/cc f)
  (p-apply f (p-cons (p-reify-conti k) '()) env k err))

(p-deffn (type-of val)
  (k (p-obj-type val)))

(p-deffn (= . xs)
  (if (not (apply eq? (map p-obj-type xs)))
        (k '())
        (let loop ((yardstick (car xs)) (maybe=s (cdr xs)))
          (cond ((null? maybe=s)
                  (k #t))
                ((p= yardstick (car maybe=s))
                  (loop yardstick (cdr maybe=s)))
                (else (k '()))))))
