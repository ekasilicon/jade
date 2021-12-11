#lang racket/base
(require racket/match
         racket/cmdline
         racket/port
         racket/pretty
         "extract.rkt"
         (prefix-in d: "../../src/disassemble.rkt")
         "../../src/unconstrained-parameter-analysis.rkt")

(command-line
 #:program "jade test"
 #:args (action net-id)
 (match action
   ["disassemble"
    (let ([count (for/fold ([count 0])
                           ([path (in-list (directory-list net-id #:build? #t))])
                   (cond
                     [(file-extract path 'approval-program)
                      => (λ (bs)
                           (time (d:disassemble-bytes bs))
                           (add1 count))]
                     [else
                      count]))])
      (printf "disassembled ~a program~a\n"
              count
              (if (= count 1) "" "s")))]
   ["unconstrained-parameter-analysis"
    (let ([count (for/fold ([count 0])
                           ([path (in-list (directory-list net-id #:build? #t))])
                   (displayln path)
                   (analyze/json-package (call-with-input-file path port->bytes)
                                         (hasheq))
                   (add1 count))])
      (printf "performed unconstrained parameter analysis on ~a program~a\n"
              count
              (if (= count 1) "" "s")))]
   [_
    (printf "expected <action> of 'disassemble' or 'unconstrained-paramter-analysis'\n")
    (exit 255)]))


