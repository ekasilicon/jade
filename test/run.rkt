#lang racket/base
(require racket/match
         racket/port
         racket/pretty)

(require "../src/parse.rkt")

(for-each
 (λ (path)
   (displayln path)
   (pretty-print
    (parse (call-with-input-file path port->string))))
 (directory-list "showcase" #:build? #t)) 

(require "algoexplorer/extract.rkt"
         "../src/disassemble.rkt"
         "../src/unconstrained-property-analysis.rkt")

(for-each
 (λ (path)
   (displayln path)
   (void (time (disassemble (file-extract path 'approval-program))))
   (let ([bs (call-with-input-file path port->bytes)])
     (with-handlers (#;[exn:fail? (λ (e) (displayln (exn-message e)))]
                        )
       (pretty-print (time (analyze/json-package bs (hash)))))))
 (directory-list "algoexplorer/mainnet" #:build? #t))

