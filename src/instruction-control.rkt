#lang racket/base
(require "static/object.rkt"
         "static/sumtype.rkt"
         "instruction.rkt")

(define instruction-control-mixins
  (list (inc ()
             [offset
              (sumtype-case-lambda Instruction1
                [(bnz offset) offset]
                #:otherwise (λ (_) #f))]
             [offset-map
              (λ (f instr)
                (sumtype-case Instruction1 instr
                  [(bnz offset) (bnz [offset (f offset)])]
                  #:otherwise values))]
             [has-inorder-successor?
              (sumtype-case-lambda Instruction1
                [(err) #f]
                #:otherwise (λ (_) #t))])
        (inc ()
             [offset
              (sumtype-case-lambda Instruction2
                [(Instruction1 instr)
                 ((super offset) instr)]
                [(b offset) offset]
                [(bz offset) offset]
                #:otherwise (λ (_) #f))]
             [offset-map
              (λ (f instr)
                (sumtype-case Instruction2 instr
                  [(Instruction1 instr)
                   ((super offset-map) f instr)]
                  [(b offset) (b [offset (f offset)])]
                  [(bz offset) (bz [offset (f offset)])]
                  #:otherwise values))]
             [has-inorder-successor?
              (sumtype-case-lambda Instruction2
                [(Instruction1 instr)
                 ((super has-inorder-successor?) instr)]
                [(return) #f]
                [(b) #f]
                #:otherwise (λ (_) #t))])
        (inc ()
             [offset
              (sumtype-case-lambda Instruction3
                [(Instruction2 instr)
                 ((super offset) instr)]
                #:otherwise (λ (_) #f))]
             [offset-map
              (λ (f instr)
                (sumtype-case Instruction3 instr
                  [(Instruction2 instr)
                   ((super offset-map) f instr)]
                  #:otherwise values))]
             [has-inorder-successor?
              (sumtype-case-lambda Instruction3
                [(Instruction2 instr)
                 ((super has-inorder-successor?) instr)]
                #:otherwise (λ (_) #t))])
        (inc ()
             [offset
              (sumtype-case-lambda Instruction4
                [(Instruction3 instr)
                 ((super offset) instr)]
                [(callsub offset) offset]
                #:otherwise (λ (_) #f))]
             [offset-map
              (λ (f instr)
                (sumtype-case Instruction4 instr
                  [(Instruction3 instr)
                   ((super offset-map) f instr)]
                  [(callsub offset) (callsub [offset (f offset)])]
                  #:otherwise values))]
             [has-inorder-successor?
              (sumtype-case-lambda Instruction4
                [(Instruction3 instr)
                 ((super has-inorder-successor?) instr)]
                [(retsub) #f]
                #:otherwise (λ (_) #t))])
        (inc ()
             [offset
              (sumtype-case-lambda Instruction5
                [(Instruction4 instr)
                 ((super offset) instr)]
                #:otherwise (λ (_) #f))]
             [offset-map
              (λ (f instr)
                (sumtype-case Instruction5 instr
                  [(Instruction4 instr)
                   ((super offset-map) f instr)]
                  #:otherwise values))]
             [has-inorder-successor?
              (sumtype-case-lambda Instruction5
                [(Instruction4 instr)
                 ((super has-inorder-successor?) instr)]
                #:otherwise (λ (_) #t))])
        (inc ()
             [offset
              (sumtype-case-lambda Instruction6
                [(Instruction5 instr)
                 ((super offset) instr)]
                #:otherwise (λ (_) #f))]
             [offset-map
              (λ (f instr)
                (sumtype-case Instruction6 instr
                  [(Instruction5 instr)
                   ((super offset-map) f instr)]
                  #:otherwise values))]
             [has-inorder-successor?
              (sumtype-case-lambda Instruction6
                [(Instruction5 instr)
                 ((super has-inorder-successor?) instr)]
                #:otherwise (λ (_) #t))])))

(provide instruction-control-mixins)
