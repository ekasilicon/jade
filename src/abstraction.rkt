#lang racket/base
(require racket/match
         racket/set
         "static/object.rkt"
         (prefix-in i: "instruction.rkt"))

(define (present? f c)
  (letrec ([present? (match-lambda
                       [(== f)
                        #t]
                       [`(,_ ,_ . ,cs)
                        (ormap present? cs)]
                       [_
                        #f])])
    (present? c)))

(define (must-be-=? X Y)
  (or (equal? X Y)))

(define (must-be-≠? X Y)
  (or (and (bytes? X)
           (bytes? Y)
           (not (bytes=? X Y)))
      (and (exact-nonnegative-integer? X)
           (exact-nonnegative-integer? Y)
           (not (= X Y)))))

(define present%
  (inc (key
        assume/present refute/present)
       [assume
        (λ (c)
          (if (present? key c)
            (assume/present c)
            (dot (unit) (unit))))]
       [refute
        (λ (c)
          (if (present? key c)
            (refute/present c)
            (dot (unit) (unit))))]))

(define application-id%
  (mix (inc ()
            [key
             (i:ApplicationID)]
            [initialize-context
             (dot (transaction-property-put)
                  (transaction-property-put 'application-id #f))]
            [assume/present
             (match-lambda
               [`(== 0 ,(i:ApplicationID) ,(? exact-nonnegative-integer? X))
                (dot (unit >>= mzero
                       transaction-property-get transaction-property-put)
                     (>>= (transaction-property-get 'application-id)
                          (match-lambda
                            [#f
                             (transaction-property-put 'application-id `(= ,X))]
                            [`(= ,Y)
                             (if (must-be-≠? X Y)
                               mzero
                               (unit))]
                            [`(≠ ,Y)
                             (if (must-be-=? X Y)
                               mzero
                               (unit))])))]               
               [`(== 0 ,(? exact-nonnegative-integer? X) ,(i:ApplicationID))
                (assume/present `(== 0 ,(i:ApplicationID) ,X))]
               [c
                (dot (log) (log "unknown ApplicationID constraint ~a" c))])]
            [refute/present
             (match-lambda
               [`(== 0 ,(i:ApplicationID) ,(? exact-nonnegative-integer? X))
                (dot (unit >>= mzero
                       transaction-property-get transaction-property-put)
                     (>>= (transaction-property-get 'application-id)
                          (match-lambda
                            [#f
                             (transaction-property-put 'application-id `(≠ ,X))]
                            [`(= ,Y)
                             (if (must-be-=? X Y)
                               mzero
                               (unit))]
                            [`(≠ ,Y)
                             (unit)])))]
               [`(== 0 ,(? exact-nonnegative-integer? X) ,(i:ApplicationID))
                (refute/present `(== 0 ,(i:ApplicationID) ,X))]
               [c
                (dot (log) (log "unknown ApplicationID constraint ~a" c))])])
       present%))

(provide application-id%)

(define rekey-to%
  (mix (inc ()
            [key
             (i:RekeyTo)]
            [initialize-context
             (dot (transaction-property-put)
                  (transaction-property-put 'rekey-to #f))]
            [assume/present
             (match-lambda
               [`(== 0 ,(i:RekeyTo) ,X)
                (dot (unit >>= mzero
                       transaction-property-get transaction-property-put)
                     (>>= (transaction-property-get 'rekey-to)
                          (match-lambda
                            [#f
                             (transaction-property-put 'rekey-to X)]
                            [Y
                             (if (must-be-≠? X Y)
                               mzero
                               (unit))])))]
               [`(== 0 ,X ,(i:RekeyTo))
                (assume/present `(== 0 ,(i:RekeyTo) ,X))]
               [c
                (dot (log) (log "unknown RekeyTo constraint ~a" c))])]
            [refute/present
             (match-lambda
               [`(== 0 ,(i:RekeyTo) ,X)
                (dot (unit >>= mzero
                       transaction-property-get transaction-property-put)
                     (>>= (transaction-property-get 'rekey-to)
                          (match-lambda
                            [#f
                             (transaction-property-put 'rekey-to X)]
                            [Y
                             (if (must-be-=? X Y)
                               mzero
                               (unit))])))]
               [`(== 0 ,X ,(i:RekeyTo))
                (refute/present `(== 0 ,(i:RekeyTo) ,X))]
               [c
                (dot (log) (log "unknown RekeyTo constraint ~a" c))])])
       present%))

(provide rekey-to%)

(define on-completion%
  (mix (inc ()
            [key
             (i:OnCompletion)]
            [initialize-context
             (dot (transaction-property-put)
                  (transaction-property-put 'on-completion (seteqv 0 1 2 4 5)))]
            [assume/present
             (match-lambda
               [`(== 0 ,(i:OnCompletion) ,(? exact-nonnegative-integer? X))
                (dot (unit >>= mzero
                       transaction-property-get transaction-property-put)
                     (>>= (transaction-property-get 'on-completion)
                          (λ (ocs)
                            (let ([ocs (set-intersect ocs (seteqv X))])
                              (if (set-empty? ocs)
                                mzero
                                (transaction-property-put 'on-completion ocs))))))]
               [`(== 0 ,(? exact-nonnegative-integer? X) ,(i:OnCompletion))
                (assume/present `(== 0 ,(i:OnCompletion) ,X))]
               [c
                (dot (log) (log "unknown OnCompletion constraint ~a" c))])]
            [refute/present
             (match-lambda
               [`(== 0 ,(i:OnCompletion) ,(? exact-nonnegative-integer? X))
                (dot (unit >>= mzero
                       transaction-property-get transaction-property-put)
                     (>>= (transaction-property-get 'on-completion)
                          (λ (ocs)
                            (let ([ocs (set-intersect ocs (set-subtract (seteqv 0 1 2 4 5) (seteqv X)))])
                              (if (set-empty? ocs)
                                mzero
                                (transaction-property-put 'on-completion ocs))))))]
               [`(== 0 ,(? exact-nonnegative-integer? X) ,(i:OnCompletion))
                (refute/present `(== 0 ,(i:OnCompletion) ,X))]
               [c
                (dot (log) (log "unknown OnCompletion constraint ~a" c))])])
       present%))

(provide on-completion%)

(define concrete-stack%
  (inc (unit >>= >>
        panic
        get put upd)
       [push
        (λ (x) (upd 'stack (λ (stk) (cons x stk)) (list)))]
       [pop
        (>>= (get 'stack)
             (match-lambda
               [(cons x stk)
                (>> (put 'stack stk)
                    (unit x))]
               [(list)
                (panic "tried to pop an empty stack")]))]))

(define concrete-cblock%
  (inc (get put)
       [get-intcblock
        (get 'intcblock)]
       [put-intcblock
        (λ (xs) (put 'intcblock xs))]
       [get-bytecblock
        (get 'bytecblock)]
       [put-bytecblock
        (λ (bss) (put 'bytecblock bss))]))

(define standard-in-mode%
  (inc (unit >>= panic
        get put)
       [in-mode
        (λ (target-mode info)
          (>>= (get 'mode #f)
               (match-lambda
                 [#f
                  (put 'mode target-mode)]
                 [mode
                  (if (eq? mode target-mode)
                    (unit)
                    (panic "need to be in mode ~a for ~a but in mode ~a" target-mode info mode))])))]))

(provide concrete-stack%
         concrete-cblock%
         standard-in-mode%)
