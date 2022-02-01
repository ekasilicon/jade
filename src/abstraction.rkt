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
            #;
            [assess
             (dot (unit >>=
                   transaction-property-get)
                  (>>= (transaction-property-get 'rekey-to)
                       (λ (ocs)
                         (unit "the RekeyTo assessment"))))]
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
            #;
            [assess
             (dot (unit >>=
                   transaction-property-get)
                  (>>= (transaction-property-get 'on-completion)
                       (λ (ocs)
                         (unit "the OnCompletion assessment"))))]
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

#|


(define txn:rekey-to%
  (inc (unit >>= >>
        mzero
        transaction-property-get transaction-property-put)
       [⊓
        (λ (key X)
          (if (eq? key 'rekey-to)
            (>>= (transaction-property-get 'rekey-to)
                 (match-lambda
                   [#f
                    (transaction-property-put 'rekey-to X)]
                   [`(= ,Y)
                    ; make no attempt to refine
                    (unit)]))
            ((super ⊓) key X)))]
       [initialize-context
        (>> (transaction-property-put 'rekey-to #f)
            (super initialize-context))]
       [assume
        (λ (c)
          (>> (if (present? (i:RekeyTo) c)
                (letrec ([loop (match-lambda
                                 [`(== 0 ,(i:RekeyTo) ,X)
                                  (>>= (transaction-property-get 'rekey-to)
                                       (match-lambda
                                         [#f
                                          (transaction-property-put 'rekey-to `(= ,X))]
                                         [`(= ,Y)
                                          ; make no attempt to see whether they *must* be different
                                          ; could be less conservative, such as if they are both concrete
                                          (unit)]))]
                                 [`(== 0 ,X ,(i:RekeyTo))
                                  (loop `(== 0 ,(i:RekeyTo) ,X))])])
                  (loop c))
                (unit))
              ((super assume) c)))]
       [refute
        (λ (c)
          (>> (if (present? (i:RekeyTo) c)
                (letrec ([loop (match-lambda
                                 [`(== 0 ,(i:RekeyTo) ,X)
                                  (>>= (transaction-property-get 'rekey-to)
                                       (match-lambda
                                         [#f
                                          ; don't remember what it's *not* equal to
                                          (unit)]
                                         [`(= ,Y)
                                          ; they are the same expression => they have the same value
                                          (if (equal? X Y)
                                            mzero
                                            (unit))]))]
                                 [`(== 0 ,X ,(i:RekeyTo))
                                  (loop `(== 0 ,(i:RekeyTo) ,X))])])
                  (loop c))
                (unit))
              ((super refute) c)))]))



(define (present? f t)
  (letrec ([present? (match-lambda
                       [(== f)
                        #t]
                       [`(,_ ,_ . ,ts)
                        (ormap present? ts)]
                       [_
                        #f])])
    (present? t)))

(define on-completion%
  #;
  (let ([key (i:OnCompletion)]
        [⊤ (seteqv 0 1 2 4 5)]
        [⊤? (λ (x) (equal? ⊤ x))]
        [⊓ (λ (oc₁)
             (>>= (transaction-property-get (i:OnCompletion))
                  (λ (oc₀)
                    (let ([oc (set-intersect oc₀ oc₁)])
                      (if (set-empty? oc)
                        mzero
                        (transaction-property-set (i:OnCompletion) oc))))))]))
  (inc (unit)
       [initialize-context
        (>> (put on-completion (seteqv 0 1 2 4 5)))]
       [assume
        (λ (c)
          (>> (if (present? key c)
                (letrec ([loop (match-lambda
                                 [`(== 0 ,(? exact-nonnegative-integer? x) ,(i:OnCompletion))
                                  (loop `(== 0 ,(i:OnCompletion) ,x))]
                                 [`(== 0 ,(i:OnCompletion) ,(? exact-nonnegative-integer? x))
                                  (refine (seteqv x))])])
                  (loop c))
                (unit))
              ((super assume) c)))]
       [refute
        (λ (c)
          (>> (if (present? (i:OnCompletion) c)
                (letrec ([loop (match-lambda
                                 [`(== 0 ,(? exact-nonnegative-integer? x) ,(i:OnCompletion))
                                  (loop `(== 0 ,(i:OnCompletion) ,x))]
                                 [`(== 0 ,(i:OnCompletion) ,(? exact-nonnegative-integer? x))
                                  (refine (set-subtract (seteqv 0 1 2 4 5) (seteqv x)))])])
                  (loop c))
                (unit))
              ((super refute) c)))]))


#;
(inc (unit >>
             assume refute)
            [assume/property
             (λ (c)
               (foldl
                (λ (I m)
                  (>> m
                      (if (present? (I 'key) c)
                        ((I 'assume) c)
                        (unit))))
                (unit)
                interpretations)
               (>> (if (present? (i:OnCompletion) c)
                     (letrec ([loop (match-lambda
                                      [`(== 0 ,(? exact-nonnegative-integer? oc) ,(i:OnCompletion))
                                       (loop `(== 0 ,(i:OnCompletion) ,oc))]
                                      [`(== 0 ,(i:OnCompletion) ,(? exact-nonnegative-integer? oc))
                                       (λ (ς ctx)
                                         (match-let ([(list txn glbl glbl-state) ctx])
                                           (let ([ocs (set-intersect (hash-ref txn (i:OnCompletion)) (seteqv oc))])
                                             (if (set-empty? ocs)
                                               (list)
                                               (list (underway [values (list)] ς [ctx (list (hash-set txn (i:OnCompletion) ocs)
                                                                                            glbl
                                                                                            glbl-state)]))))))])])
                       (loop c))
                     (unit))
                   (if (present? (i:RekeyTo) c)
                     (match c
                       [`(== 0 ,(i:RekeyTo) ,v₀)
                        (λ (ς ctx)
                          (match-let ([(list txn glbl glbl-state) ctx])
                            (match (hash-ref txn (i:RekeyTo))
                              [`(???)
                               (list (underway [values (list)] ς [ctx (list (hash-set txn (i:RekeyTo) `(isa ,v₀))
                                                                            glbl
                                                                            glbl-state)]))]
                              [`(not ,v)
                               ((refute `(== 0 ,v₀ ,v)) ς ctx)]
                              [`(isa ,v)
                               ((assume `(== 0 ,v₀ ,v)) ς ctx)])))])
                     (unit))
                   (unit)))]
            [refute/property
             (λ (c)
               (>> (if (present? (i:OnCompletion) c)
                     (letrec ([loop (match-lambda
                                      [`(== 0 ,(? exact-nonnegative-integer? oc) ,(i:OnCompletion))
                                       (loop `(== 0 ,(i:OnCompletion) ,oc))]
                                      [`(== 0 ,(i:OnCompletion) ,(? exact-nonnegative-integer? oc))
                                       (λ (ς ctx)
                                         (match-let ([(list txn glbl glbl-state) ctx])
                                           (let ([ocs (set-intersect (hash-ref txn (i:OnCompletion)) (set-subtract (seteqv 0 1 2 4 5) (seteqv oc)))])
                                             (if (set-empty? ocs)
                                               (list)
                                               (list (underway [values (list)] ς [ctx (list (hash-set txn (i:OnCompletion) ocs)
                                                                                            glbl
                                                                                            glbl-state)]))))))])])
                       (loop c))
                     (unit))
                   (if (present? (i:RekeyTo) c)
                     (match c
                       [`(== 0 ,(i:RekeyTo) ,v₀)
                        (λ (ς ctx)
                          (match-let ([(list txn glbl glbl-state) ctx])
                            (match (hash-ref txn (i:RekeyTo))
                              [`(???)
                               (list (underway [values (list)] ς [ctx (list (hash-set txn (i:RekeyTo) `(not ,v₀))
                                                                            glbl
                                                                            glbl-state)]))]
                              [`(not ,v)
                               (list (underway [values (list)] ς [ctx (list (hash-set txn (i:RekeyTo) `(not ,v₀))
                                                                            glbl
                                                                            glbl-state)]))]
                              [`(isa ,v)
                               ((refute `(== 0 ,v₀ ,v)) ς ctx)])))])
                     (unit))
                   (unit)))])
; there are a few kinds of abstractions

; a subset abstraction

; start X at (set 0 1 2 4 5), which are the possible values
; value is always some subset of this
; assume (= X n) means intersect X with { n }
; refute (= X n) means intersect X with the complement of { n }
; if the set is empty, there is no possible value and that branch is invalid
; if the set is not empty, there is a possible value
; some weird programs might actually operate numerically
; like check whether it is < 3 or something
; assume (< X 3) means intersect X with (set 0 1 2 4 5) that are less than 3
; so one way of viewing these expressions is as a predicate, and we simply intersect
; the current set with the predicate (applied to the whole set)

; the single value abstraction

; suppose we know that the set of possible values is finite, but we don't know them up front.
; we have a distinguished value ⊤ which means it can be anything
; we want to come to the value from the bottom and the top
; we have two sets, the values which it cannot be and the values which it can be
; of course, those cannot overlap, so we always check the appropriate set before continuing
|#

