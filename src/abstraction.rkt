#lang racket/base
(require racket/match
         #;racket/set
         "static/object.rkt"
         (prefix-in i: "instruction/opcode.rkt")
         )

(define (present? f c)
  (letrec ([present? (match-lambda
                       [(== f)
                        #t]
                       [`(,_ ,_ . ,cs)
                        (ormap present? cs)]
                       [_
                        #f])])
    (present? c)))


(define application-id%
  (inc ()
       [initialize-context
        (dot (transaction-property-put)
             (transaction-property-put 'application-id #f))]
       [assume
        (λ (c o)
          ((if (present? (i:ApplicationID) c)
             (dot (unit) (unit))
             (dot (unit) (unit)))
           o))]
       [refute
        (λ (c o)
          ((if (present? (i:ApplicationID) c)
             (dot (unit) (unit))
             (dot (unit) (unit)))
           o))]))

(provide application-id%)

#|

(define transaction-property
  (inc (unit >>= >>)
       [transaction-property-get
        (λ (key)
           (λ (ς ctx)
             (match-let ([(list txn glbl glbl-state) ctx])
               (list (underway [values (list (hash-ref txn key))] ς ctx)))))]
       [transaction-property-put
        (λ (key val)
          (λ (ς ctx)
            (match-let ([(list txn glbl glbl-state) ctx])
              (list (underway [values (list)] ς [ctx (list (hash-set txn key val)
                                                           glbl
                                                           glbl-state)])))))]))

(define txn:application-id%
  (inc (unit >>= >>
        mzero
        transaction-property-get transaction-property-put)
       [⊓
        (λ (key X)
          (if (eq? key 'application-id)
            (>>= (transaction-property-get 'application-id)
                 (match-lambda
                   [#f
                    (transaction-property-put 'application-id X)
                    #;
                    (match X
                      [`(≠ 0)
                       `(≠ 0)]
                      [`(= 0)]
                      [])
                    #;
                    (transaction-property-put 'application-id X)]
                   [`(≠ 0)
                    (match X
                      [`(≠ 0)
                       (unit)]
                      [`(= 0)
                       mzero])]
                   [`(= 0)
                    (match X
                      [`(= 0)
                       (unit)]
                      [`(≠ 0)
                       mzero])]))
            ((super ⊓) key X)))]
       [initialize-context
        (>> (transaction-property-put 'application-id #f)
            (super initialize-context))]
       [assume
        (λ (c)
          (>> (if (present? (i:ApplicationID) c)
                (match c
                  [`(== 0 ,(i:ApplicationID) 0)
                   (⊓ 'application-id `(= 0))]
                  [`(== 0 0 ,(i:ApplicationID))
                   (⊓ 'application-id `(= 0))])
                (unit))
              ((super assume) c)))]
       [refute
        (λ (c)
          (>> (if (present? (i:ApplicationID) c)
                (match c
                  [`(== 0 ,(i:ApplicationID) 0)
                   (⊓ 'application-id `(≠ 0))]
                  [`(== 0 0 ,(i:ApplicationID))
                   (⊓ 'application-id `(≠ 0))])
                (unit))
              ((super refute) c)))]))

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

(define txn:on-completion%
  (inc (unit >>= >>
        mzero
        transaction-property-get transaction-property-put)
       [⊓
        (λ (key X)
          (if (eq? key 'on-completion)
            (>>= (transaction-property-get 'on-completion) 
                 (λ (oc)
                   (let ([oc (set-intersect oc X)])
                     (if (set-empty? oc)
                       mzero
                       (transaction-property-put 'on-completion oc)))))
            ((super ⊓) key X)))]
       [initialize-context
        (>> (transaction-property-put 'on-completion (seteqv 0 1 2 4 5))
            (super initialize-context))]
       [assume
        (λ (c)
          (>> (if (present? (i:OnCompletion) c)
                (letrec ([loop (match-lambda
                                 [`(== 0 ,(? exact-nonnegative-integer? x) ,(i:OnCompletion))
                                  (loop `(== 0 ,(i:OnCompletion) ,x))]
                                 [`(== 0 ,(i:OnCompletion) ,(? exact-nonnegative-integer? x))
                                  (⊓ 'on-completion (seteqv x))])])
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
                                  (⊓ 'on-completion (set-subtract (seteqv 0 1 2 4 5) (seteqv x)))])])
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

