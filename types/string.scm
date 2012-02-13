
(define-predicate p-string?)
(define-operation (scheme-string str))
(define quo (->string (integer->char 34))) ;"

(define make-p-string
  (case-lambda
    ((str)
      (make-p-string str 'utf-8))
    ((str enc)
      (p-obj
        ((p-string? x) #t)
        ((p-obj-type x) 'string)
        ((p-literal x)
          (string-append quo
            (apply string-append (map (lambda (c)
              (let ((s (->string c)) (i (char->integer c)))
                (cond ((string=? s quo) (string-append "\\" quo))
                      ((<= 32 i 126) s)
                      ((= i  0) "\\0")
                      ((= i  7) "\\a")
                      ((= i  8) "\\b")
                      ((= i  9) "\\t")
                      ((= i 10) "\\n")
                      ((= i 12) "\\f")
                      ((= i 13) "\\r")
                      ((= i 27) "\\e")
                      (else (string-append "\\u" (fmt #f (pad-char #\0 (pad/left 4 (num i 16))))))))) (string->list str)))
            quo))
        ((scheme-string x) str)
        ((p= x y)
          (string=? str (scheme-string y)))))))
