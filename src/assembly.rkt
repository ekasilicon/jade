#lang racket/base
(require racket/match)

(define emit-directive
  (match-lambda))

(define (emit! directives)
  (for-each
   emit-directive!
   directives))

(provide emit-directive!
         emit!)

(module+ main
  (emit! (disassemble-port (current-input-port))))
