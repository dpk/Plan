
(use socket foreign)

(define (socketpair domain type protocol)
  (define socketpair-2
    (foreign-lambda int "socketpair" int int int int))
  )
