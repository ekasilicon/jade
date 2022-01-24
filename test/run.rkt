#lang racket/base
(require racket/match
         racket/set
         racket/port
         racket/pretty)

(require "algoexplorer/extract.rkt"
         "../src/error.rkt"
         ;"../src/parse.rkt"
         "../src/disassemble.rkt"
         "../src/unconstrained-property-analysis.rkt"
         "../src/debug.rkt")

(match (current-command-line-arguments)
  #;
  [(vector "showcase")
   (for-each
    (λ (path)
      (displayln path)
      (let ([rs (time (analyze/assembly (call-with-input-file path port->string)))])
        (for ([r (in-set rs)])
          (pretty-print r))))
    (directory-list "showcase" #:build? #t))]
  [(vector "algoexplorer" net)
   (pretty-print
    (for/fold ([results (hasheq)])
              ([path (in-list (directory-list (build-path "algoexplorer" net) #:build? #t))])
      (displayln path)
      #;(void (time (disassemble (file-extract path 'approval-program))))
      (match (analyze/json-package (call-with-input-file path port->bytes) (hash))
        [(error-result tag message)
         (displayln tag)
         (displayln message)
         (hash-update results tag add1 0)]
        [rs
         (if (zero? (set-count rs))
           (begin
             (displayln 'no-states)
             (displayln "no states")
             (hash-update results 'no-states add1 0))
           (hash-update results 'successes add1 0)
             #;
             (begin
               
               (pretty-print (disassemble (file-extract path 'approval-program))))
             #;
               (begin
                 (displayln n)
                 (for ([r (in-set rs)])
                   (pretty-print r))))])))]
  [(vector "debug" paths ...)
   (for-each
    (λ (path)
      (displayln path)
      (debug (disassemble (file-extract path 'approval-program))))
    paths)])

 





