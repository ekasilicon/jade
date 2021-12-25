#lang racket/base
(require racket/match
         (only-in racket/string string-join))

(define ((unit . xs) input i sk fk) (sk xs i fk))
(define (fail input i sk fk) (fk))

(define ((>>= m f) input i sk fk)
  (m input i (λ (xs i fk) ((apply f xs) input i sk fk)) fk))

(define (>> m . ms)
  (foldl (λ (m m₀) (>>= m₀ (λ _ m))) m ms))

(define (>>0 m . ms)
  (>>= m (λ xs (foldr (λ (m m₀) (>> m m₀)) (apply unit xs) ms))))

(define (lift f p)
  (>>= p (λ xs (call-with-values (λ () (apply f xs)) unit))))

(define ((∨ . ps) input i sk fk) 
  (let loop ([ps ps])
    (match ps
      [(list)
       (fk)]
      [(cons p ps)
       (p input i sk (λ () (loop ps)))])))

(define (∘ . ps)
  (let loop ([ps ps])
    (match ps
      [(list)
       (unit)]
      [(cons p ps)
       (>>= p (λ (x) (>>= (loop ps) (λ xs (apply unit x xs)))))])))

(define (p* p)
  (∨ (>>= p (λ (x) (>>= (p* p) (λ (xs) (unit (cons x xs))))))
     (unit (list))))

(define (p+ p)
  (>>= p (λ (x) (>>= (p* p) (λ (xs) (unit (cons x xs)))))))

(define (pn n p)
  (if (zero? n)
    (unit (list))
    (>>= p (λ (x) (>>= (pn (sub1 n) p) (λ (xs) (unit (cons x xs))))))))

(define (p? p)
  (∨ p (unit #f)))

(define ((p- p₀ p₁) input i sk fk₀)
  (p₁ input i (λ (xs i fk₁) (fk₀)) (λ () (p₀ input i sk fk₀))))

(define (end-of-input input i sk fk)
  (if (= (string-length input) i)
    (sk (list #f) i fk)
    (fk)))

(define (read-char input i sk fk)
  (if (= (string-length input) i)
    (fk)
    (sk (list (string-ref input i)) (add1 i) fk)))

(define (cc ?)
  (>>= read-char
       (λ (c)
         (if (? c)
           (unit c)
           fail))))

(define (c c₀)
  (cc (λ (c) (eqv? c c₀))))

(define (read-chars n ?)
  (pn n (cc ?)))

(define (^^ s)
  (let ([cs (string->list s)])
    (λ (c) (memv c cs))))

(define (literal s)
  (let loop ([cs (string->list s)])
    (match cs
      [(list)
       (unit s)]
      [(cons c₀ cs)
       (>> (c c₀) (loop cs))])))

(define space* (p* (c #\space)))
(define space+ (p+ (c #\space)))

(define whitespace* (p* (∨ (c #\space) (c #\tab))))
(define whitespace+ (p+ (∨ (c #\space) (c #\tab))))

(define newline
  (∨ (c #\newline)
     (>> (c #\return)
         (c #\newline))))

(define line-sentinel
  (∨ newline end-of-input))

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

(define ((guard p expected . expecteds) input i sk _)
  (p input i sk (λ () (error (apply report input i expected expecteds)))))

; testing
(define-syntax-rule (parse-success p-expr s v ...)
  (let ([p p-expr])
    (p s 0
       (λ (vs i fk)
         (match vs
           [(list v ...)
            (void)]
           [vs
            (error 'parse-success "parser ~a expected ~v but got ~v" p (list 'v ...) vs)]))
       (λ ()
         (error 'parse-success "parser ~a expected ~v but failed" p (list 'v ...))))))

(define-syntax-rule (parse-failure p-expr s rgx)
  (let ([p p-expr])
    (with-handlers ([exn:fail? (λ (e)
                                 (unless (regexp-match? rgx (exn-message e))
                                   (error 'parse-failure
                                          "parser ~a expected to fail with message matching ~v but failed with message ~s"
                                          p
                                          rgx
                                          (exn-message e))))])
      (p s 0 (λ (vs i fk) (error 'parse-failure "parser ~a expected to fail but succeeded with ~a" p vs)) void))))

(provide (all-defined-out))

(module+ test
  (parse-success (∘ (p- (p* (cc void)) (c #\b)) (c #\b))
                 "there is no letter b in this"
                 (app list->string "there is no letter ")
                 (app string "b")))
