
(define (p-read port err)
  (if (string? port)
        (p-read (open-input-string port) err)
        (begin
          (p-rm-wc port)
          (let ((c (peek-char port)))
            (case (peek-char port)
              ((#\')
                (read-char port)
                (p-cons 'quote (p-cons (p-read port err) '())))
              ((#\`)
                (read-char port)
                (p-cons 'quasiquote (p-cons (p-read port err) '())))
              ((#\,)
                (read-char port)
                (if (eq? #\@ (peek-char port))
                      (begin
                        (read-char port)
                        (p-cons 'unquote-splicing (p-cons (p-read port err) '())))
                      (p-cons 'unquote (p-cons (p-read port err) '()))))
              ((#\()
                (p-read-cons port err))
              ((#\" #\") ; sorry, my syntax highlighter chokes on #\" alone
                (p-read-string port err))
              (else
                (if (eof-object? c)
                      (err 'unterminated-expression "unterminated expression for reader")
                      (p-read-sym/num port err))))))))
(define (p-rm-wc port) ; remove whitespace/comments
  (case (peek-char port)
        ((#\space #\tab #\newline #\return #\nul)
          (read-char port)
          (p-rm-wc port))
        ((#\;)
          (read-line port)
          (p-rm-wc port))
        (else port)))

(define (p-read-cons port err)
  (read-char port) ; swallow the open-paren
  (p-read-inside-cons port err))
(define (p-read-inside-cons port err)
  (p-rm-wc port)
  (case (peek-char port)
    ((#\.)
      (read-char port)
      (p-rm-wc port)
      (let ((datum (p-read port err)))
        (p-rm-wc port) (read-char port) ; swallow close-paren
        datum))
    ((#\&)
      (read-char port)
      (let ((datum (p-read-sym/num port err)))
        (p-rm-wc port) (read-char port) ; swallow close-paren
        datum))
    ((#\))
      (read-char port) ; swallow the close-paren
      '())
    (else ; this code depends on left-to-right order of evaluation for function args.
          ; this works in Chicken (and many other schemes) but is not defined in the spec.
          ; so chicken might decide in the future to change it.
      (p-cons (p-read port err) (p-read-inside-cons port err)))))

(define (p-read-sym/num port err)
  (case (peek-char port)
    ((#\:)
      (read-char port)
      (string->symbol (scheme-string (p-read-string port err))))
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
      (read port)) ; for now we'll use Scheme's reader
    (else
      (let ((val (read port)))
        (cond ((eq? val 'nil) '())
              ((eq? val 't) #t)
              (else val))))))

(define (p-read-string port err) ; 'conses' too much: should be destructive to the start str
  (read-char port) ; swallow the "
  (let loop ((str "") (next-chr (peek-char port)))
    (case next-chr
      ((#\\)
        (loop (string-append str (p-read-bsl-esc port err)) (peek-char port))) ; depends on ltr arg evaluation
      ((#\" #\")
        (read-char port)
        (make-p-string str 'utf-8))
      (else
        (loop (string-append str (->string (read-char port))) (peek-char port))))))
(define (p-read-bsl-esc port err)
  (read-char port) ; swallow the \
  (let ((c (read-char port)))
    (case c
      ((#\n) "\n")
      ((#\r) "\r")
      ((#\t) "\t")
      ((#\s) " ")
      ((#\b) "\b")
      ((#\f) "\f")
      ((#\a) "\a")
      ((#\0) "\u0000")
      ((#\e) "\x1b")
      ((#\x)
        (let ((char1 (read-char port)) (char2 (read-char port))) ; ltr dependent
          (if (not (and (hex-digit? char1) (hex-digit? char2)))
                (err 'invalid-escape "must follow \\x with two hex digits")
                (->string (integer->char (string->number (string-append (->string char1) (->string char2)) 16))))))
      ((#\u)
        (let ((char1 (read-char port)) (char2 (read-char port)) (char3 (read-char port)) (char4 (read-char port))) ; ltr dependent
          (if (not (and (hex-digit? char1) (hex-digit? char2) (hex-digit? char3) (hex-digit? char4)))
                (err 'invalid-escape "must follow \\u with four hex digits")
                (->string (integer->char (string->number (string-append (->string char1) (->string char2) (->string char3) (->string char4)) 16))))))
      (else (->string c)))))
(define (hex-digit? c)
  (if (memq (char-downcase c) '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f)) #t #f))
