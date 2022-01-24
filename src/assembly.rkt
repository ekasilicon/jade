#lang racket/base
(require (only-in racket/match match-lambda)
         "static/record.rkt"
         "static/sumtype.rkt"
         "static/object.rkt")

(define-sumtype Pseudoinstruction
  (instruction instruction)
  (varuint-immediate value)
  (bytes-immediate value))

(provide (sumtype-out Pseudoinstruction))

(define-sumtype Directive
  (pragma content)
  (label ℓ)
  Pseudoinstruction)

(provide (sumtype-out Directive))

(record assembly (logic-sig-version directives))

(provide assembly)

#|
(require (only-in racket/string string-join)
         "instruction/show.rkt")

(define directives-show
  (match-lambda
    [(directives logic-sig-version directives)
     (let ([instruction-show (fix (instruction-show/version logic-sig-version))])
       (string-join (cons (format "#pragma version ~a"
                                  logic-sig-version)
                          (map
                           (sumtype-case-lambda Directive
                             [(pragma content)
                              (string-append "#pragma " content)]
                             [(label ℓ)
                              (format "~a:" ℓ)]
                             [(instruction instruction)
                              (string-append "  " ((instruction-show 'instruction-show) instruction))])
                           directives))
                    "\n"))]))

(define resolve-labels
  (match-lambda
    [(directives logic-sig-version directives)
     ]))

#;
(fix (inc ()
                       [control-flow-graph
                        (cons lsv (resolve-control-flow lsv directives))]
                       [assembly
                        (let ([instruction-show (fix (mix (instruction-show/version lsv)))])
                          )]))
|#
