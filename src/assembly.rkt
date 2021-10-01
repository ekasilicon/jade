#lang racket/base
(require racket/match)

(define emit-directive!
  (match-lambda
    [`(pragma ,content)
     (printf "#pragma ~a\n" content)]
    [`(label ,ℓ)
     (printf "label ~a:\n" ℓ)]
    [(cons name args)
     (printf "  ~a" name)
     (for-each
      (λ (arg)
        (cond
          [(symbol? arg)
           (printf " ~a" arg)]
          [(bytes? arg)
           (printf " ~v" arg)]
          [(exact-nonnegative-integer? arg)
           (printf " ~a" arg)]
          [else
           (print arg)
           (error 'emit-directive "unknown type")]))
      args)
     (newline)]))

(define (emit! directives)
  (for-each
   emit-directive!
   directives))

(provide emit-directive!
         emit!)

(module+ main
  (require "disassemble.rkt")
  (emit! (disassemble-port (current-input-port))))
