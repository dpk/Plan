
(include "./plan.scm")

(define stdin* (open-input-file* fileno/stdin))
(define stdout* (open-output-file* fileno/stdout))
(define stderr* (open-output-file* fileno/stderr))

(set-signal-handler! signal/int (lambda (n) (exit)))

(if (terminal-port? stdin*)
      (p-repl)
      (let loop ((exp (p-read stdin* repl-eof-errh)))
        (run exp toplevel-env)
        (loop (p-read stdin* repl-eof-errh))))
