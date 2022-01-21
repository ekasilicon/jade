#lang racket/base
(require (only-in racket/list take)
         (only-in racket/match match match-lambda)
         "../static/sumtype.rkt"
         "../static/object.rkt"
         "../version.rkt"
         "opcode.rkt")

(define instruction-control/version
  (make-*/version
   'instruction-control/version
   (inc ())
   (inc ()
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
            ((super 'offset) instr)]
           [(b offset) offset]
           [(bz offset) offset]
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
        [offset
         (sumtype-case-lambda Instruction3
           [(Instruction2 instr)
            ((super 'offset) instr)]
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
        [offset
         (sumtype-case-lambda Instruction4
           [(Instruction3 instr)
            ((super 'offset) instr)]
           [(callsub offset) offset]
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
        [offset
         (sumtype-case-lambda Instruction5
           [(Instruction4 instr)
            ((super 'offset) instr)]
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
        [offset
         (sumtype-case-lambda Instruction6
           [(Instruction5 instr)
            ((super 'offset) instr)]
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
        [offset
         (match-lambda
           [(varuint-immediate) #f]
           [(bytes-immediate) #f]
           [instr ((super 'offset) instr)])]
        [offset-map
         (λ (f instr)
           (match instr
             [(varuint-immediate value) (varuint-immediate value)]
             [(bytes-immediate value) (bytes-immediate value)]
             [instr ((super 'offset-map) f instr)]))]
        [has-inorder-successor?
         (match-lambda
           [(varuint-immediate) #t]
           [(bytes-immediate) #t]
           [instr ((super 'has-inorder-successor?) instr)])])))

(provide instruction-control/version)

(define (resolve-CFG-placeholders lsv phs initial-ph)
  (define-values (offset-map has-inorder-successor?)
    (let ([o (fix (instruction-control/version lsv))])
      (values (o 'offset-map)
              (o 'has-inorder-successor?))))
  ; this loop does a pass to fix instructions whose successor
  ; isn't simply the next instruction
  ; these instructions include terminal instructions,
  ; such as err, return, and retsub,
  ; and branching instructions,
  ; such as bnz, bz, and callsub.
  (let loop ([ph initial-ph])
    (match (placeholder-get ph)
      [(list)
       (void)]
      [(cons instr next-ph)
       (let ([instr (offset-map
                     (λ (offset)
                       (cond
                         [(hash-ref phs offset #f)
                          => values]
                         [else
                          (error 'control "invalid offset ~a" offset)]))
                     instr)])
         (placeholder-set!
          ph
          (cons instr
                ; instructions such as return, b, and err
                ; never proceed to the next instruction
                (if (has-inorder-successor? instr)
                  next-ph
                  (list)))))
       (loop next-ph)]))
  (make-reader-graph initial-ph))

(provide resolve-CFG-placeholders)



