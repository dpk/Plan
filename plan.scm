
(use numbers extras posix srfi-69 operations fmt)

(define-syntax p-obj
  (syntax-rules ()
    ((_ body ...) (join (object #f body ...) (make-p-obj)))))

(define-syntax p-set
  (syntax-rules ()
    ((_ name val) (p-env-set toplevel-env 'name val))))
(define-syntax p-set!
  (syntax-rules ()
    ((_ name val) (p-env-set! toplevel-env 'name val))))
(define (p-fn proc)
  (make-p-fn '() proc toplevel-env))
(define (p-prim proc)
  (make-p-prim '() proc))
(define-syntax p-deffn
  (er-macro-transformer (lambda (exp rename compare)
    (let ((name (caadr exp))
          (args (cdadr exp))
          (body (cddr exp)))
      `(p-set! ,name (p-fn
        (lambda (args env k err)
          (apply (lambda ,args ,@body) (plan->scheme-list args)))))))))
(define-syntax p-defprim
  (er-macro-transformer (lambda (exp rename compare)
    (let ((name (caadr exp))
          (args (cdadr exp))
          (body (cddr exp)))
      `(p-set! ,name (p-prim
        (lambda (args env k err)
          (apply (lambda ,args ,@body) (plan->scheme-list args))))))))) ; change to -list when we work out why that isn't currently working

(include "./types.scm")

(define-operation (p-eval exp env k err)
  (if (p-symbol? exp)
        (let ((val (p-env-lookup env exp)))
          (if (eq? val no-binding*)
                (err 'unbound-symbol
                     (string-append "unbound symbol: " (symbol->string exp)))
                (k val)))
       (k exp)))
(define-operation (p-literal exp)
  (cond
    ((eq? exp #t) "t")
    ((eq? exp '()) "nil")
    ((symbol? exp)
      (let* ((ss (symbol->string exp)) (ss-list (string->list ss)))
        (if (or (eq? (car ss-list) #\:)
                (eq? exp 'nil)
                (eq? exp 't)) (string-append ":" (p-literal (make-p-string ss))) ss)))
    ((number? exp) (number->string exp))
    (else (string-append "%{Scheme Object: " (with-output-to-string (lambda () (display exp))) "}"))))
(define-operation (tags obj) (make-hash-table))
(define-operation (p-obj-type x)
  (cond ((number? x) 'number)
        ((symbol? x) 'symbol)
        ((null? x) 'nil)
        ((eq? x #t)) 't))
(define-operation (p= x y)
  (cond ((number? x)
          (= x y))
        ((symbol? x)
          (eq? x y))
        ((null? x)
          (null? y))
        ((eq? x #t)
          (eq? y #t))))

(define (plan->scheme-list obj)
  (if (p-cons? obj)
        (cons (p-car obj) (plan->scheme-list (p-cdr obj)))
        obj))
(define (scheme->plan-list obj)
  (if (pair? obj)
        (p-cons (car obj) (scheme->plan-list (cdr obj)))
        obj))

(define (scheme->plan obj)
  (cond ((pair? obj) (p-cons (scheme->plan (car obj)) (scheme->plan (cdr obj))))
        ((string? obj) (make-p-string obj))
        ((or (symbol? obj) (number? obj) (null? obj) (eq? #t obj)) obj)))
(define (plan->scheme obj)
  (cond ((p-cons? obj) (cons (plan->scheme (p-car obj)) (plan->scheme (p-cdr obj))))
        ((p-string? obj) (scheme-string obj))
        ((or (symbol? obj) (number? obj) (null? obj) (eq? #t obj)) obj)))

(define (make-p-obj)
  (let ((tag-table (make-hash-table)))
    (object #f
      ((tags x) tag-table)
      ((p-obj-type x) 'object)
      ((p-literal x)
        (string-append "#<" (symbol->string (p-obj-type x))
          (if (hash-table-exists? (tags x) 'name)
                (string-append " " (symbol->string (hash-table-ref (tags x) 'name))) "") ">"))
      ((print-object x port) (with-output-to-port port
        (lambda () (display (string-append "#<Plan Object: " (p-literal x) ">"))))))))

(define (run exp env)
  (p-execute (compile exp env) env (lambda (x) x) top-error-handler))

(define (p-eval-each exps env k err)
  (p-eval (p-car exps) env (lambda (evxp)
    (if (null? (p-cdr exps))
          (k evxp)
          (p-eval-each (p-cdr exps) env k err))) err))

(define (p-eval-map exps env k err)
  (if (not (p-cons? exps))
        (k exps)
        (p-eval (p-car exps) env (lambda (evxp)
          (p-eval-map (p-cdr exps) env (p-cons-k evxp k) err)) err)))
(define (p-execute-map exps env k err)
  (if (not (p-cons? exps))
        (k exps)
        (p-execute (p-car exps) env (lambda (exxp)
          (p-execute-map (p-cdr exps) env (p-cons-k exxp k) err)) err)))
(define (p-cons-k h k)
  (lambda (t)
    (k (p-cons h t))))

(define toplevel-env (make-p-env (make-hash-table) '()))
(define (top-error-handler sym str)
  (display (string-append "Error: " str " (" (symbol->string sym) ")\n")) '())

(define (repl-eof-errh sym str)
  (if (eq? 'unterminated-expression sym)
        (exit)
        (top-error-handler sym str)))

(define stdin* (open-input-file* fileno/stdin))
(define p-repl (case-lambda
  (() (p-repl toplevel-env))
  ((env)
    (display "> ")
    (let ((exp (p-read stdin* repl-eof-errh)))
      (display (p-literal (run exp env))) (newline))
    (p-repl env))))

(include "./compile.scm")
(include "./read.scm")
(include "./lib.scm")
