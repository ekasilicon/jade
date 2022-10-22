#lang racket/base
(require (only-in racket/list take)
         (only-in racket/match match match-lambda)
         "../static/sumtype.rkt"
         "../static/object.rkt"
         "../version.rkt"
         "../instruction.rkt")

(define instruction-control/version
  (make-*/version
   'instruction-control/version
   (inc ())
   (inc ()
        [offsets
         (sumtype-case-lambda Instruction1
           [(bnz offset) (list offset)]
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
        [offsets
         (sumtype-case-lambda Instruction2
           [(Instruction1 instr)
            ((super 'offsets) instr)]
           [(b offset) (list offset)]
           [(bz offset) (list offset)]
           #:otherwise (λ (_) #f))]
        [offset-map
         (λ (f instr)
           (sumtype-case Instruction2 instr
             [(Instruction1 instr)
              ((super 'offset-map) f instr)]
             [(b offset) (b [offset (f offset)])]
             [(bz offset) (bz [offset (f offset)])]
             #:otherwise values))]
        [has-inorder-successor?
         (sumtype-case-lambda Instruction2
           [(Instruction1 instr)
            ((super 'has-inorder-successor?) instr)]
           [(return) #f]
           [(b) #f]
           #:otherwise (λ (_) #t))])
   (inc ()
        [offsets
         (sumtype-case-lambda Instruction3
           [(Instruction2 instr)
            ((super 'offsets) instr)]
           #:otherwise (λ (_) #f))]
        [offset-map
         (λ (f instr)
           (sumtype-case Instruction3 instr
             [(Instruction2 instr)
              ((super 'offset-map) f instr)]
             #:otherwise values))]
        [has-inorder-successor?
         (sumtype-case-lambda Instruction3
           [(Instruction2 instr)
            ((super 'has-inorder-successor?) instr)]
           #:otherwise (λ (_) #t))])
   (inc ()
        [offsets
         (sumtype-case-lambda Instruction4
           [(Instruction3 instr)
            ((super 'offsets) instr)]
           [(callsub offset) (list offset)]
           #:otherwise (λ (_) #f))]
        [offset-map
         (λ (f instr)
           (sumtype-case Instruction4 instr
             [(Instruction3 instr)
              ((super 'offset-map) f instr)]
             [(callsub offset) (callsub [offset (f offset)])]
             #:otherwise values))]
        [has-inorder-successor?
         (sumtype-case-lambda Instruction4
           [(Instruction3 instr)
            ((super 'has-inorder-successor?) instr)]
           [(retsub) #f]
           #:otherwise (λ (_) #t))])
   (inc ()
        [offsets
         (sumtype-case-lambda Instruction5
           [(Instruction4 instr)
            ((super 'offsets) instr)]
           #:otherwise (λ (_) #f))]
        [offset-map
         (λ (f instr)
           (sumtype-case Instruction5 instr
             [(Instruction4 instr)
              ((super 'offset-map) f instr)]
             #:otherwise values))]
        [has-inorder-successor?
         (sumtype-case-lambda Instruction5
           [(Instruction4 instr)
            ((super 'has-inorder-successor?) instr)]
           #:otherwise (λ (_) #t))])
   (inc ()
        [offsets
         (sumtype-case-lambda Instruction6
           [(Instruction5 instr)
            ((super 'offsets) instr)]
           #:otherwise (λ (_) #f))]
        [offset-map
         (λ (f instr)
           (sumtype-case Instruction6 instr
             [(Instruction5 instr)
              ((super 'offset-map) f instr)]
             #:otherwise values))]
        [has-inorder-successor?
         (sumtype-case-lambda Instruction6
           [(Instruction5 instr)
            ((super 'has-inorder-successor?) instr)]
           #:otherwise (λ (_) #t))])
   (inc ()
        [offsets
         (sumtype-case-lambda Instruction7
           [(Instruction6 instr)
            ((super 'offsets) instr)]
           #:otherwise (λ (_) #f))]
        [offset-map
         (λ (f instr)
           (sumtype-case Instruction7 instr
             [(Instruction6 instr)
              ((super 'offset-map) f instr)]
             #:otherwise values))]
        [has-inorder-successor?
         (sumtype-case-lambda Instruction7
           [(Instruction6 instr)
            ((super 'has-inorder-successor?) instr)]
           #:otherwise (λ (_) #t))])
   (inc ()
        [offsets
         (sumtype-case-lambda Instruction8
           [(Instruction7 instr)
            ((super 'offsets) instr)]
           [(switch offsets) offsets]
           #:otherwise (λ (_) #f))]
        [offset-map
         (λ (f instr)
           (sumtype-case Instruction8 instr
             [(Instruction7 instr)
              ((super 'offset-map) f instr)]
             [(switch offsets) (switch [offsets (map f offsets)])]
             #:otherwise values))]
        [has-inorder-successor?
         (sumtype-case-lambda Instruction8
           [(Instruction7 instr)
            ((super 'has-inorder-successor?) instr)]
           [(switch) #t]
           #:otherwise (λ (_) #t))])
   (inc ())))

(provide instruction-control/version)
