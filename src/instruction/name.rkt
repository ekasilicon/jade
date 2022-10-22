#lang racket/base
(require (only-in racket/match match-lambda)
         "../static/sumtype.rkt"
         "../static/object.rkt"
         "../version.rkt"
         "../instruction.rkt")

(define instruction-name/version
  (make-*/version
   'instruction-name/version
   (inc ())
   (inc ()
        [instruction-name
         (sumtype-case-lambda Instruction1
           #:otherwise (λ (_) "<instruction-name>"))])
   (inc ()
        [instruction-name
         (sumtype-case-lambda Instruction2
           [(Instruction1 instr) 
            ((super 'instruction-name) instr)]
           #:otherwise (λ (_) "<instruction-name>"))])
   (inc ()
        [instruction-name
         (sumtype-case-lambda Instruction3
           [(Instruction2 instr) 
            ((super 'instruction-name) instr)]
           #:otherwise (λ (_) "<instruction-name>"))])
   (inc ()
        [instruction-name
         (sumtype-case-lambda Instruction4
           [(Instruction3 instr) 
            ((super 'instruction-name) instr)]
           #:otherwise (λ (_) "<instruction-name>"))])
   (inc ()
        [instruction-name
         (sumtype-case-lambda Instruction5
           [(Instruction4 instr) 
            ((super 'instruction-name) instr)]
           #:otherwise (λ (_) "<instruction-name>"))])
   (inc ()
        [instruction-name
         (sumtype-case-lambda Instruction6
           [(Instruction5 instr) 
            ((super 'instruction-name) instr)]
           #:otherwise (λ (_) "<instruction-name>"))])
   (inc ()
        [instruction-name
         (sumtype-case-lambda Instruction7
           [(Instruction6 instr) 
            ((super 'instruction-name) instr)]
           #:otherwise (λ (_) "<instruction-name>"))])
   (inc ()
        [instruction-name
         (sumtype-case-lambda Instruction8
           [(Instruction7 instr) 
            ((super 'instruction-name) instr)]
           #:otherwise (λ (_) "<instruction-name>"))])
   (inc ())))

(provide instruction-name/version)

(module+ main
  (((fix (instruction-name/version 1))
    'instruction-name)
   (err)))
