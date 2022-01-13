#lang racket/base
(require racket/match
         racket/set
         racket/port
         racket/pretty)

(require "../src/parse.rkt"
         "algoexplorer/extract.rkt"
         "../src/disassemble.rkt"
         "../src/unconstrained-property-analysis.rkt"
         "../src/debug.rkt")

(match (current-command-line-arguments)
  [(vector "showcase")
   (for-each
    (λ (path)
      (displayln path)
      (pretty-print
       (parse (call-with-input-file path port->string))))
    (directory-list "showcase" #:build? #t))]
  [(vector "algoexplorer")
   (for-each
    (λ (path)
      (displayln path)
      (void (time (disassemble (file-extract path 'approval-program))))
      (let ([bs (call-with-input-file path port->bytes)])
        (with-handlers ([exn:fail? (λ (e) (displayln (exn-message e)))])
          (let* ([rs (analyze/json-package bs (hash))]
                 [n (set-count rs)])
            (if (zero? n)
              (begin
                (displayln "no states")
                (pretty-print (disassemble (file-extract path 'approval-program))))
              (begin
                (displayln n)
                (for ([r (in-set rs)])
                  (pretty-print r))))))))
    (directory-list "algoexplorer/mainnet" #:build? #t))]
  [(vector "debug" paths ...)
   (for-each
    (λ (path)
      (displayln path)
      (debug (disassemble (file-extract path 'approval-program))))
    paths)])

 





