#lang racket/base
(require racket/match
         (only-in racket/string string-join))

(define (parse p input)
  (p input 0 (λ (xs i fk)
               (if (= (string-length input) i)
                 xs
                 (fk)))
     (λ () (error 'parse "parse error"))))

(define-syntax-rule (parse-success p-expr s v ...)
  (with-handlers ([exn:fail? (λ (e) (error 'parse-success "parser ~a expected ~v but failed" 'p-expr (list 'v ...)))])
    (call-with-values
     (λ () (parse p-expr s))
     (match-lambda
       [(list v ...)
        (void)]
       [vs
        (error 'parse-success "parser ~a expected ~v but got ~v" 'p-expr (list 'v ...) vs)]))))

(define-syntax-rule (parse-failure p-expr s rgx)
  (with-handlers ([exn:fail? (λ (e)
                               (unless (regexp-match? rgx (exn-message e))
                                 (error 'parse-failure
                                        "parser ~a expected to fail with message matching ~v but failed with message ~s"
                                        'p-expr
                                        rgx
                                        (exn-message e))))])
    (call-with-values
     (λ () (parse p-expr s))
     (λ vs (error 'parse-failure "parser ~a expected to fail but succeeded with ~a" 'p-expr vs)))))

(define-syntax-rule (delay p)
  (λ (input i sk fk) (p input i sk fk)))

(define ((unit . xs) input i sk fk) (sk xs i fk))

(module+ test
  (parse-success (unit 42) "" 42))

(define (fail input i sk fk) (fk))

(module+ test
  (parse-failure fail "" #px""))

(define ((>>= m f) input i sk fk)
  (m input i (λ (xs i fk) ((apply f xs) input i sk fk)) fk))

(define (>> m . ms)
  (foldl (λ (m m₀) (>>= m₀ (λ _ m))) m ms))

(define (>>0 m . ms)
  (>>= m (λ xs (foldr (λ (m m₀) (>> m m₀)) (apply unit xs) ms))))

(define (fmap f . ps)
  (>>= (foldr (λ (p m) (>>= p (λ (x) (>>= m (λ (xs) (unit (cons x xs)))))))
              (unit (list))
              ps)
       (λ (xs) (unit (apply f xs)))))

(define (charp input i sk fk)
  (if (= (string-length input) i)
    (fk)
    (sk (list (string-ref input i)) (add1 i) fk)))

(define (?c ?)
  (>>= charp (λ (c) (if (? c) (unit c) fail))))

(define (mc s)
  (let ([cs (string->list s)])
    (?c (λ (c) (memv c cs)))))

(define (cp c₀) (?c (λ (c) (eqv? c c₀))))

(define (litp s)
  (fmap
     list->string
     (foldr
      (λ (c₀ p) (fmap cons (cp c₀) p))
      (unit (list))
      (string->list s))))

(module+ test
  (parse-success (litp "hello") "hello" "hello"))

(define ((∨ . ps) input i sk fk) 
  ((foldr (λ (p fk) (λ () (p input i sk fk))) fk ps)))

(define (⋆p p)
  (∨ (fmap cons p (delay (⋆p p)))
     (unit (list))))

(module+ test
  (parse-success (fmap list->string (⋆p (cp #\space))) "   " "   "))

(define (+p p)
  (fmap cons p (⋆p p)))

(define (np n p)
  (if (zero? n)
    (unit (list))
    (fmap cons p (np (sub1 n) p))))

(define space* (⋆p (∨ (cp #\space) (cp #\tab))))
(define space+ (+p (∨ (cp #\space) (cp #\tab))))

(define (delimitp delimiterp p)
  (∨ (fmap cons p (⋆p (>> delimiterp p)))
     (unit (list))))

(define (?p p)
  (∨ p (unit #f)))

(define ((\\p p₀ p₁) input i sk fk₀)
  (p₁ input i (λ (xs i fk₁) (fk₀)) (λ () (p₀ input i sk fk₀))))

; reporting

(define ((trace which p) input i sk fk)
  (displayln 'TRACE)
  (displayln which)
  (println p)
  (println i)
  (println (snippet input i))
  (p input i (λ (x i fk)
               (println x)
               (sk x i fk))
     (λ ()
       (displayln "FAILED")
       (fk))))

(define ((checkpoint template . args) input i sk fk)
  (display "checkpoint ")
  (displayln (apply format template args))
  (println (snippet input i))
  (sk (list) i fk))

(define (snippet input i)
  (substring input i (min (string-length input) (+ i 24))))

(define (report input i expected . expecteds)
  (format #<<REPORT
parse failure

  at position ~a
  encountered ~s
  
but expected~a

~a
REPORT
          i
          (let ([s (format "~s" (snippet input i))])
            (substring s 1 (max 1 (sub1 (string-length s)))))
          (if (null? expecteds) "" " one of")
          (string-join (map (λ (expected) (string-append "  " expected "\n"))
                            (cons expected expecteds))
                       "")))

(define ((guard p expected . expecteds) input i sk fk)
  (p input i sk (λ () (error (apply report input i expected expecteds)))))

(provide (all-defined-out))
