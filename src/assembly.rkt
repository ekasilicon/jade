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
