#lang racket/base
(require (only-in racket/match match-lambda)
         "../static/sumtype.rkt"
         "../static/object.rkt"
         "../version.rkt"
         "opcode.rkt")

(define instruction-version/version
  (make-*/version
   'instruction-version/version
   (inc ())
   (inc ()
        [instruction-version
         (sumtype-case-lambda Instruction1
           #:otherwise (λ (_) 1))])
   (inc ()
        [instruction-version
         (sumtype-case-lambda Instruction2
           [(Instruction1 instr) 
            ((super 'instruction-version) instr)]
           #:otherwise (λ (_) 2))])
   (inc ()
        [instruction-version
         (sumtype-case-lambda Instruction3
           [(Instruction2 instr) 
            ((super 'instruction-version) instr)]
           #:otherwise (λ (_) 3))])
   (inc ()
        [instruction-version
         (sumtype-case-lambda Instruction4
           [(Instruction3 instr) 
            ((super 'instruction-version) instr)]
           #:otherwise (λ (_) 4))])
   (inc ()
        [instruction-version
         (sumtype-case-lambda Instruction5
           [(Instruction4 instr) 
            ((super 'instruction-version) instr)]
           #:otherwise (λ (_) 5))])
   (inc ()
        [instruction-version
         (sumtype-case-lambda Instruction6
           [(Instruction5 instr) 
            ((super 'instruction-version) instr)]
           #:otherwise (λ (_) 6))])
   (inc ()
        [instruction-version
         (match-lambda
           [(varuint-immediate) #f]
           [(bytes-immediate) #f]
           [instr ((super 'instruction-version) instr)])])))

(provide instruction-version/version)

(module+ main
  (((fix (instruction-version/version 1))
    'instruction-version)
   (err)))
