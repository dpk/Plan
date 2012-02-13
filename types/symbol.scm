
(define p-symbol? symbol?)

(define (p-symbol x)
  (cond ((p-symbol? x) x)
        ((string? x) (string->symbol x))))
