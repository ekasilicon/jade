#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/pretty)
         racket/promise
         racket/stxparam)

(define-syntax-parameter super (syntax-rules ()))

(define-syntax field
  (syntax-parser
    [(_ myself mysuper (f:id ...) e)
     (local-expand/capture-lifts
      #'(let-syntax ([f (let ([cached #f])
                          (syntax-parser
                            [(f . rst)
                             #'(#%app f . rst)]
                            [f:id
                             (or cached
                                 (with-syntax ([x (syntax-local-lift-expression #'(delay (myself 'f)))])
                                   (set! cached #'(force x))
                                   cached))]))]
                     ...)
          (syntax-parameterize ([super (let ([cached (hasheq)])
                                         (syntax-parser
                                           [(_ x:id)
                                            (let ([x-symbol (syntax->datum #'x)])
                                              (or (hash-ref cached x-symbol #f)
                                                  (with-syntax ([y (syntax-local-lift-expression #'(delay (mysuper 'x)))])
                                                    (set! cached (hash-set cached x-symbol #'(force y)))
                                                    (hash-ref cached x-symbol))))]))])
            e))
      'expression
      (list))]))

(define-syntax inc
  (syntax-parser
    [(_ (d:id ...) [f:id e] ...)
     (with-syntax ([(g ...) (foldl (λ (g gs) (cons g (remove g gs free-identifier=?))) (syntax->list #'(d ...)) (syntax->list #'(f ...)))])
       #'(λ (myself mysuper)
           (λ (msg)
             (case msg
               [(f) (field myself mysuper (g ...) e)]
               ...
               [else
                (mysuper msg)]))))]))

(define (fix p [b (λ (msg) (error 'message "unrecognized: ~a" msg))])
  (letrec ([o (p (λ (x) (o x)) b)])
    o))

(define (mix . ps)
  (foldr (λ (p p₀) (λ (self super) (p self (p₀ self super)))) (λ (self super) super) ps))



(provide super inc mix fix)



#|


#;
(mix (y)
     [x (+ x y y 2)])

(mixin
    >>=
    [>> (λ (m . ms) (foldl (λ (m m₀) (>>= m₀ (λ _ m))) m ms))])

(mixin
    unit
    >>=
  [read-byte (λ (σ)
               )])

(let ([else #f])
  (cond
    [else 20]
    [#t 30]))


(let ([x 10])
  (let-syntax ([x (λ (stx) #'x)])
    (x)))


(define f0
  (λ (self super)
    (λ (n)
      (if (zero? n)
        1
        (super n)))))

(define fn
  (λ (self super)
    (λ (n)
      (* n (self (- n 1))))))

(let ([fp (λ (self super) (f0 self (fn self super)))])
  (letrec ([f (fp (λ xs (apply f xs)) raise)])
    (f 100)))


#|
(define x3
  (λ (self super)
    (λ (msg)
      (case msg
        [(a) (* (super 'a) 2)]
        [else (super msg)]))))


(define x2
  (λ (self super)
    (λ (msg)
      (case msg
        [(a) (+ (super 'a) 2)]
        [else (super msg)]))))

(define x1
  (λ (self super)
    (λ (msg)
      (case msg
        [(a) 10]
        [else (super msg)]))))

(define o0
  (λ (msg)
    (error 'object "unrecognized: ~a" msg)))

(let ([p (λ (self super) (x3 self (x2 self (x1 self super))))])
  (letrec ([o (p (λ xs (apply o xs)) o0)])
    (o 'a)))
|#

(define x3
  (λ (super)
    (λ (self)
      (λ (msg)
        (case msg
          [(a) (* (super 'a) 2)]
          [else (super msg)])))))


(define x2
  (λ (super)
    (λ (self)
      (λ (msg)
        (case msg
          [(a) (+ (super 'a) 2)]
          [else (super msg)])))))

(define x1
  (λ (super)
    (λ (self)
      (λ (msg)
        (case msg
          [(a) 10]
          [else (super msg)])))))

(define o0
  (λ (msg)
    (error 'object "unrecognized: ~a" msg)))

(let ([p (λ (super) (λ (self) ((x3 ((x2 ((x1 super) self)) self)) self)))])
  (letrec ([o ((p o0) (λ xs (apply o xs)))])
    (o 'a)))

#;
(let ([p (λ (self super) (x3 self (x2 self (x1 self super))))])
  (letrec ([o (p (λ xs (apply o xs)) o0)])
    (o 'a)))
|#
