#lang racket/base
(require racket/match
         racket/set)

(struct state (pc stk xxx) #:transparent)

(struct partial (xs st) #:transparent)
(struct failure (msg) #:transparent)
(struct success () #:transparent)

(define (append-map f xs)
  (match xs
    [(list)
     (list)]
    [(cons x xs)
     (append (f x) (append-map f xs))]))

(define ((unit . xs) lsv bc pc stk xxx)
  (list (partial xs (state pc stk xxx))))
(define ((fail template . args) lsv bc pc stk xxx) 
  (list (failure (apply format template args))))
(define ((succeed) lsv bc pc stk xxx)
  (list (success)))
(define ((each . ms) lsv bc pc stk xxx) 
  (append-map (λ (m) (m lsv bc pc stk xxx)) ms))
(define ((>>= m f) lsv bc pc stk xxx)
  (append-map
   (match-lambda
     [(partial xs (state pc stk xxx))
      ((apply f xs) lsv bc pc stk xxx)]
     [x (list x)])
   (m lsv bc pc stk xxx)))
(define (>> m . ms)
  (foldl (λ (m M) (>>= M (λ _ m))) m ms))

(define (read-byte lsv bc pc stk xxx)
  (if (and (>= pc 0)
           (< pc (bytes-length bc)))
    (list (partial (list (bytes-ref bc pc))
                   (state (add1 pc) stk xxx)))
    (list (failure "out of range"))))

(require racket/unit
         "../components/read-encoded-monadic-unit.rkt")

(define-values/invoke-unit/infer read-encoded-monadic@)

(define (io i o)
  (let loop ([σ (hasheq)]
             [i i])
    (match i
      [(list)
       (let loop ([σ σ]
                  [o o])
         (match o
           [(list)
            (unit)]
           [(cons τA o)
            (>> (push (match τA
                        [(or 'bytes 'uint64)
                         τA]))
                (loop σ o))]))]
      [(cons τA i)
       (>>= pop
            (λ (τ)
              (match τA
                [(or 'bytes 'uint64)
                 (if (eq? τA τ)
                   (loop σ i)
                   (fail "expected type ~a but got type ~a" τA τ))]
                [X
                 (cond
                   [(hash-ref σ X #f)
                    => (λ (τA)
                         (if (eq? τA τ)
                           (loop σ i)
                           (fail "expected type ~a but got type ~a" τA τ)))]
                   [else
                    (loop (hash-set σ X τ)
                          i)])])))])))

(define ((jump offset) lsv bc pc stk xxx) 
  (if (and (< offset 0)
           (<= lsv 3))
    (list (failure "backwards jump"))
    (let ([dst (+ pc offset)])
      (if (or (< dst 0)
              (> dst (bytes-length bc))
              (and (= lsv 1)
                   (= dst (bytes-length bc))))
        (list (failure "bad jump destination"))
        (list (partial (list) (state dst stk xxx)))))))

(define (transaction-field-type i)
  (unit (match i
          [0  'bytes]  ; Sender
          [16 'uint64] ; TypeEnum
          [22 'uint64] ; GroupIndex
          [25 'uint64] ; OnCompletion
          [32 'bytes]  ; RekeyTo
          )))

(define (check-opcode oc)
  (displayln oc)
  (match oc
    [#x0f ; >=
     (io '(uint64 uint64) '(uint64))]
    [#x11 ; ||
     (io '(uint64 uint64) '(uint64))]
    [#x12 ; ==
     (io '(A A) '(uint64))]
    [#x17 ; btoi
     (io '(bytes) '(uint64))]
    [#x19 ; |
     (io '(uint64 uint64) '(uint64))]
    [#x20 ; intcblock
     read-intcblock]
    [#x22 ; intc_0
     (push 'uint64)]
    [#x23 ; intc_1
     (push 'uint64)]
    [#x24 ; intc_2
     (push 'uint64)]
    [#x26 ; bytecblock
     read-bytecblock]
    [#x27 ; bytec
     (>> read-uint8
         (push 'bytes))]
    [#x28 ; bytec_0
     (push 'bytes)]
    [#x29 ; bytec_1
     (push 'bytes)]
    [#x2a ; bytec_2
     (push 'bytes)]
    [#x2b ; bytec_3
     (push 'bytes)]
    [#x31 ; txn
     (>>= (>>= read-uint8 transaction-field-type)
          push)]
    [#x32 ; global
     (>>= (>>= read-uint8
               (λ (i)
                 (unit (match i
                         [3 'bytes]  ; ZeroAddress
                         [4 'uint64] ; GroupSize
                         ))))
          push)]
    [#x33 ; gtxn
     (>>= (>>= read-uint8 (λ (gi) (>>= read-uint8 transaction-field-type)))
          push)]
    [#x35 ; store
     (>>= read-uint8
          (λ (i) (>>= pop (λ (τ) (store i τ)))))]
    [#x37 ; gtxna
     (>>= (>>= read-uint8
               (λ (gi)
                 (>>= read-uint8
                      (λ (fi)
                        (>>= read-uint8
                             (λ (ai)
                               (unit (match fi
                                       [26 ; ApplicationArgs
                                        'bytes]))))))))
          push)]
    [#x41 ; bz
     (>>= read-int16
          (λ (offset)
            (each (unit)
                  (jump offset))))]
    [#x42 ; b
     (>>= read-int16 jump)]
    [#x43 ; return
     (>>= pop
          (match-lambda
            ['uint64 (succeed)]
            ['bytes (fail "returned with bytes")]))]
    [#x44 ; assert
     (>>= pop
          (λ (A)
            (match A
              ['uint64
               (each (unit)
                     (fail "assertion"))])))]
    [#x48 ; pop
     pop]
    [#x49 ; dup
     (>>= pop
          (λ (A)
            (>> (push A)
                (push A))))]
    [#x51 ; substring
     (>> read-uint8
         read-uint8
         (io '(bytes) '(bytes)))]
    [#x64 ; app_global_get
     
     ]))

(define (get-pc lsv bc pc stk xxx)
  (list (partial (list pc) (state pc stk xxx))))

(define (program-length lsv bc pc stk xxx)
  (list (partial (list (bytes-length bc)) (state pc stk xxx))))

(define (get-stack lsv bc pc stk xxx)
  (list (partial (list stk) (state pc stk xxx))))

(define ((push x) lsv bc pc stk xxx)
  (list (partial (list) (state pc (cons x stk) xxx))))

(define (pop lsv bc pc stk xxx)
  (match stk
    [(list)
     (list (failure "tried to pop an empty stack"))]
    [(cons x stk)
     (list (partial (list x) (state pc stk xxx)))]))

(define pop2
  (>>= pop (λ (A) (>>= pop (λ (B) (unit A B))))))

(define ((load i) lsv bc pc stk xxx)
  (cond
    [(hash-ref xxx i #f)
     => (λ (A) (list (partial (list A) (state pc stk xxx))))]
    [else
     (list (failure (format "no value at ~a" i)))]))

(define ((store i A) lsv bc pc stk xxx)
  (list (partial (list) (state pc stk (hash-set xxx i A)))))

(define →
  (>> (>>= read-opcode check-opcode)
      (>>= get-pc
           (λ (pc)
             (>>= program-length
                  (λ (n)
                    (if (= pc n)
                      (>>= get-stack
                           (λ (stk)
                             (match stk)))
                      (unit))))))))

(define (type-check bc)
  (if (zero? (bytes-length bc))
    (error 'type-check "empty program")
    (let ([lsv (bytes-ref bc 0)])
      (let →* ([st (state 1 (list) (hasheqv))]
               [seen (set)])
        (if (set-member? seen st)
          seen
          (foldl
           (λ (r seen)
             (match r
               [(partial (list) st)
                (→* st seen)]
               [(success)
                seen]
               [(failure msg)
                (displayln msg)
                seen]))
           (set-add seen st)
           (match-let ([(state pc stk xxx) st])
             (→ lsv bc pc stk xxx))))))))

(module+ main
  (require "../../corpus/run.rkt")

  (run (λ (u name bytecode)
         (displayln name)
         (displayln (set-count (type-check bytecode)))
         (raise 'stop)
         u) #f))
