#lang racket/base
(require racket/match
         racket/pretty)

(define (read-char! ip msg)
  (let ([c (read-char ip)])
    (if (eof-object? c)
      (error 'parse msg)
      c)))

(define (peek-char! ip msg)
  (let ([c (peek-char ip)])
    (if (eof-object? c)
      (error 'parse msg)
      c)))

(define (read-identifier ip)
  (string->symbol
   (apply
    string
    (let ([c (read-char ip)])
      (if (char-alphabetic? c)
        (cons c
              (let loop ()
                (let ([c (peek-char ip)])
                  (if (or (char-alphabetic? c)
                          (char-numeric? c)
                          (and (char-punctuation? c)
                               (not (memv c '(#\( #\) #\[ #\])))))
                    (cons (read-char ip) (loop))
                    (list)))))
        (error 'parse "identifier expected to start with alphabetic, got ~v" c))))))

(define (read-line-end ip)
  (let ([c (read-char ip)])
    (unless (eqv? c #\newline)
      (error 'parse "expected newline, got ~v" c))))

(define (read-line read ip)
  (let ([x (read ip)])
    (read-line-end ip)
    x))

(define (expect ip c₀ template)
  (let ([c (read-char ip)])
    (unless (eqv? c₀ c)
      (error 'parse template c))))

(define (read-character* ip)
  (let ([c (peek-char! ip "reading character class but input ended")])
    (if (eqv? c #\})
      (list)
      (cons (read-character (read-char ip) ip)
            (read-character* ip)))))

(define (read-character c ip)
  (match c
    [#\.
     '(wildcard)]
    [#\\
     (match (read-char! ip "end of input after \\")
       [#\.
        `(literal #\.)]
       [#\r
        `(literal #\return)]
       [#\n
        `(literal #\newline)]
       [#\t
        `(literal #\tab)]
       [#\(
        `(literal #\()]
       [#\)
        `(literal #\))]
       [#\{
        `(literal #\{)]
       [#\}
        `(literal #\})]
       [#\\
        `(literal #\\)])]
    [#\{
     (let ([ccs (read-character* ip)])
       (begin0 `(∨ . ,ccs)
         (expect ip #\} (format "expected } to close ~v but got ~~a" ccs))))]
    [#\^
     `(¬ ,(read-character (read-char! ip "end of input after negation") ip))]
    [c
     `(literal ,c)]))

(define (read-gamma ip)
  (match (read-char! ip "end of input on production")
    [#\(
     (let ([id (read-identifier ip)])
       (begin0 `(A ,id)
         (expect ip #\) (format "expected ) to close ~a but got ~~a" id))))]
    [#\[
     (let ([c (read-char! ip "end of input after [")])
       (cond
         [(eqv? c #\])
          (error 'parse "empty []")]
         [else
          (begin0 (list (string->symbol (string c)) `(∘ . ,(read-gammas ip)))
            (expect ip #\] (format "expected ] to close ~a but got ~~a" c)))]))]
    [c
     `(a ,(read-character c ip))]))

(define (read-gammas ip)
  (let ([c (peek-char ip)])
    (if (or (eqv? c #\newline)
            (eqv? c #\]))
      (list)
      (cons (read-gamma ip)
            (read-gammas ip)))))

(define (read-production ip)
  `(∘ . ,(read-line read-gammas ip)))

(define (read-productions ip)
  (let ([c (peek-char ip)])
    (if (eqv? c #\newline)
      (begin
        (read-char ip)
        (list))
      (cons (read-production ip)
            (read-productions ip)))))

(define (read-non-terminal ip)
  (cons (read-line read-identifier ip)
        `(∨ . ,(read-productions ip))))

(define (read-non-terminals ip)
  (let ([p (peek-char ip)])
    (if (eof-object? p)
      (list)
      (cons (read-non-terminal ip)
            (read-non-terminals ip)))))

(define (read-grammar ip)
  (let ([nts (read-non-terminals ip)])
    (match-let ([(cons (cons start _) _) nts])
      (cons start
            (make-immutable-hash nts)))))

(define grammar
  (call-with-input-file "assembly-specification.txt" read-grammar))

(define (interpret grammar input)
  (match-let ([(cons start nts) grammar])
    (define (dispatch g i sk fk)
      #;(pretty-print g)
      (if (= i (string-length input))
        (fk)
        (match g
          [`(a ,cc)
           (let ([c (string-ref input i)])
             (if (cc-member? cc c)
               (sk c (add1 i))
               (fk)))]
          [`(∘ . ,gs)
           (letrec ([loop (λ (gs i sk)
                            (match gs
                              [(list)
                               (sk (list) i)]
                              [(cons g gs)
                               (dispatch g i (λ (x i) (loop gs i (λ (xs i) (sk (cons x xs) i)))) fk)]))])
             (loop gs i sk))]
          [`(∨ . ,gs)
           (∨ gs i sk fk)]
          [`(? ,g)
           (dispatch g i sk (λ () (sk #f i)))]
          [`(* ,g)
           (* g i sk fk)]
          [`(+ ,g)
           (+ g i sk fk)]
          [`(A ,id)
           (dispatch (hash-ref nts id) i sk fk)])))
    (define (* g i sk fk)
      (dispatch g i
                (λ (x i) (* g i (λ (xs i) (sk (cons x xs) i)) fk))
                (λ () (sk (list) i))))
    (define (+ g i sk fk)
      (dispatch g i (λ (x i) (* g i (λ (xs i) (sk (cons x xs) i)) fk)) fk))
    (define (cc-member? cc c)
      (match cc
        [`(literal ,c₀)
         (eqv? c₀ c)]
        [`(¬ ,cc)
         (not (cc-member? cc c))]
        [`(∨ . ,ccs)
         (ormap (λ (cc) (cc-member? cc c)) ccs)]))
    (define (∨ gs i sk fk)
      (match gs
        [(cons g gs)
         (dispatch g i sk (λ () (∨ gs i sk fk)))]
        [(list)
         (fk)]))
    (dispatch (hash-ref nts start) 0
              (λ (x i)
                (if (= (string-length input) i)
                  x
                  (error 'interpret "parsed but remainder of string is ~s" (substring input i))))
              (λ ()
                (error 'interpret "fail: report better")))))

(define programs
  (list
   #;
   #<<PROGRAM
#pragma version 3

PROGRAM
   #;
      #<<PROGRAM
#pragma version 3
+

PROGRAM
            #<<PROGRAM
#pragma version 3
intc 10
intc 11
+
txn Fee
txn OnCompletion

PROGRAM
))

(map
 (λ (pr) (interpret grammar pr))
 programs)

#|

weird aside

imagine this

the grammar is a program
we interpret it with an input
the result is a parse tree
we partially evaluate with respect to the grammar program, and we have a parser

but then

the grammar for the grammar program starts very rudimentary and verbose
we give directives which change the way it is parsed

for instance, to be perfectly clear, we might write
intc[space+]<uint>[space*]
as a spec for a line
but we want to be able to write
intc <uint>
interpreting a single space character #\space as [space+] and the end of the line as [space*]

so we have a kind of macro

|#


