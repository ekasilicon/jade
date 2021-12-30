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
         "../src/disassemble.rkt")

(for-each
 (λ (path)
   (displayln path)
   (void (time (disassemble (file-extract path 'approval-program)))))
 (directory-list "algoexplorer/mainnet" #:build? #t))

