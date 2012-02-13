
(define-predicate p-cons?)
(define-settable-operation (p-car x)
  (if (null? x) x))
(define-settable-operation (p-cdr x)
  (if (null? x) x))

(define (p-cons car-f cdr-f)
  (p-obj
    ((p-cons? x) #t)
    ((p-car x) car-f)
    ((p-cdr x) cdr-f)
    (((setter p-car) x new) (set! car-f new))
    (((setter p-cdr) x new) (set! cdr-f new))
    
    ((p-eval exp env k err)
      (p-eval (p-car exp) env (lambda (val)
        (cond ((p-fn? val)
                (p-eval-map (p-cdr exp) env (lambda (args) (p-apply val args env k err)) err))
              ((p-prim? val)
                (p-apply val (p-cdr exp) env k err))
              ((p-macro? val)
                (p-expand val (p-cdr exp) env (lambda (new-exp)
                  (p-eval new-exp env k err)) err))
              ((p-continuation? val)
                (p-eval (p-car (p-cdr exp)) env (lambda (arg)
                  (p-invoke val arg)) err)))) err))
    
    ((p= x y)
      (and (p= (p-car x) (p-car y)) (p= (p-cdr x) (p-cdr y))))
    
    ((p-obj-type x) 'cons)
    ((p-literal x)
      (cond ((and (eq? (p-car x) 'quote) (null? (p-cdr (p-cdr x))))
               (string-append "'" (p-literal (p-car (p-cdr x)))))
            ((and (eq? (p-car x) 'quasiquote) (null? (p-cdr (p-cdr x))))
               (string-append "`" (p-literal (p-car (p-cdr x)))))
            ((and (eq? (p-car x) 'unquote) (null? (p-cdr (p-cdr x))))
               (string-append "," (p-literal (p-car (p-cdr x)))))
            ((and (eq? (p-car x) 'unquote-splicing) (null? (p-cdr (p-cdr x))))
               (string-append ",@" (p-literal (p-car (p-cdr x)))))
            (else (string-append "(" (p-cons-literal x) ")"))))))

(define (p-cons-literal x)
  (string-append (p-literal (p-car x))
    (cond
      ((p-cons? (p-cdr x)) (string-append " " (p-cons-literal (p-cdr x))))
      ((null? (p-cdr x)) "")
      (else (string-append " . " (p-literal (p-cdr x)))))))