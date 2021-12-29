#lang racket/base
(require racket/port
         racket/pretty)

(require "../src/parse/assembly.rkt")

; disassembly
; parsing
; analysis

(for-each
 (λ (path)
   (displayln path)
   (pretty-print
    (parse (call-with-input-file path port->string))))
 (directory-list "showcase" #:build? #t)) 



#;
(require racket/match
         racket/file
         racket/runtime-path)

; 1. test against showcase programs
;    a. include the use of constant mappings
; 2. test against Reach programs
; 3. test against AlgoExplorer programs



#;
(define-runtime-path here-path ".")


#;
(define (run f u)
  (define (fold-corpus corpus-name)
    (fold-files
     (λ (name type u)
       (if (and (eq? type 'file)
                (regexp-match? #px"tealc$" name))
         (f u name (file->bytes name))
         u))
     u
     (build-path here-path corpus-name)
     #f))
  (let ([u (fold-corpus "reach-examples")])
    u))

#;
(provide run)

