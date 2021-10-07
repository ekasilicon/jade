#lang racket/base
(require racket/match
         racket/string
         racket/pretty
         "record.rkt"
         "sumtype.rkt"
         (prefix-in i: "instruction.rkt"))

; Need to add cut so that committed failure can be represented
; and better errors can be reported.

(define ((unit . xs) input i sk fk) (sk xs i fk))
(define (fail input i sk fk) (fk))

(define ((>>= m f) input i sk fk)
  (m input i (λ (xs i fk) ((apply f xs) input i sk fk)) fk))

(define (>> m . ms)
  (foldl (λ (m m₀) (>>= m₀ (λ _ m))) m ms))

(define ((lift f) . xs)
  (call-with-values (λ () (apply f xs)) unit))

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

(define ((p* p) input i sk fk) 
  (let loop ([xs (list)]
             [i i]
             [fk fk])
    (p input i (λ (x i fk)
                 (match x
                   [(list x)
                    (loop (cons x xs) i fk)]
                   [_
                    (error 'p* "expected a single result")]) )
       (λ () (sk (list (reverse xs)) i fk)))))

(define (p+ p)
  (>>= p (λ (x) (>>= (p* p) (λ (xs) (unit (cons x xs)))))))

(define (p? p)
  (∨ p (unit #f)))

(define (cc ?)
  (>>= read-char
       (λ (c)
         (if (? c)
           (unit c)
           fail))))

(define (^ s)
  (let ([cs (string->list s)])
    (λ (c) (not (memv c cs)))))

(define (^^ s)
  (let ([cs (string->list s)])
    (λ (c) (memv c cs))))

(define (end-of-input input i sk fk)
  (if (= (string-length input) i)
    (sk (list #f) i fk)
    (fk)))

(define (read-char input i sk fk)
  (if (= (string-length input) i)
    (fk)
    (sk (list (string-ref input i)) (add1 i) fk)))

(define (literal s)
  (let loop ([cs (string->list s)])
    (match cs
      [(list)
       (unit s)]
      [(cons c₀ cs)
       (>>= read-char
            (λ (c)
              (if (eqv? c₀ c)
                (loop cs)
                fail)))])))

(define (make-symbol string)
  (>>= (literal string) (lift string->symbol)))

(define space* (p* (cc (λ (c) (eqv? c #\space)))))
(define space+ (p+ (cc (λ (c) (eqv? c #\space)))))

(define whitespace* (p* (cc (λ (c) (or (eqv? c #\space) (eqv? c #\tab))))))
(define whitespace+ (p+ (cc (λ (c) (or (eqv? c #\space) (eqv? c #\tab))))))

(define (snippet input i)
  (substring input i (min (string-length input) (+ i 64))))

(define ((report p template) input i sk fk)
  (p input i sk (λ () (error template (snippet input i)))))

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

(define ((not-implemented id) input i sk fk)
  (error 'parse "not implemented at ~a and ~s" id (snippet input i)))

; numbers

(define digit-values
  (hasheqv #\0 0
           #\1 1
           #\2 2
           #\3 3
           #\4 4
           #\5 5
           #\6 6
           #\7 7
           #\8 8
           #\9 9
           #\a 10 #\A 10
           #\b 11 #\B 11
           #\c 12 #\C 12
           #\d 13 #\D 13
           #\e 14 #\E 14
           #\f 15 #\F 15))

(define octal-digit? (^^ "01234567"))
(define decimal-digit? (^^ "0123456789"))
(define hex-digit? (^^ "0123456789abcdefABCDEF"))

(define ((make-digits→numeral radix) ds)
  (foldl (λ (d n) (+ (* n radix) (hash-ref digit-values d))) 0 ds))

(define hex-varuint
  (>> (literal "0x")
      (>>= (p+ (cc hex-digit?)) (lift (make-digits→numeral 16)))))

(define octal-varuint
  (>> (literal "0")
      (>>= (p+ (cc octal-digit?)) (lift (make-digits→numeral 8)))))

(define decimal-varuint
  (>>= (p+ (cc decimal-digit?)) (lift (make-digits→numeral 10))))

(define varuint
  (∨ hex-varuint
     octal-varuint
     decimal-varuint))

(define uint8
  (report (>>= varuint
               (λ (x)
                 (if (< x 256)
                   (unit x)
                   fail)))
          "expected a number which can fit in a byte; got ~s"))

; bytes

(require file/sha1)

(define pbytes
  (∨ (>> (literal "base64")
         whitespace+
         (>>= (p* (cc (^ "\r\n")))
              (lift (λ (cs) (apply string cs)))))
     (>> (literal "0x")
         (>>= (p* (cc hex-digit?)) (lift (λ (cs) (hex-string->bytes (apply string cs))))))))

#|
bytes
byte[+ ]base64[+ ](base64)
byte[+ ]b64[+ ](base64)
byte[+ ]base64\((base64)\)
byte[+ ]b64\((base64)\)
byte[+ ]base32[+ ](base32)
byte[+ ]b32[+ ](base32)
byte[+ ]base32\((base32)\)
byte[+ ]b32\((base32)\)
byte[+ ](hex-number)
byte[+ ](string)
|#

(record pragma (content))

(define pragma-directive
  (>> (literal "#pragma ")
      (>>= (p* (cc (^ "\r\n")))
           (lift (λ (cs) (pragma [content (apply string cs)]))))))

(define (make-instruction name constructor . arguments)
  (>>= (make-symbol name)
       (λ (id)
         (>>= (let loop ([args arguments])
                (match args
                  [(list)
                   (unit (list))]
                  [(cons arg args)
                   (>> whitespace+
                       (>>= arg
                            (λ (x)
                              (>>= (loop args)
                                   (λ (xs) (unit (cons x xs)))))))]))
              (λ (xs) (unit (apply constructor xs)))))))

(require (for-syntax racket/base
                     racket/match
                     syntax/parse))

(define-syntax enumtype-parser
  (syntax-parser
    [(_ typename:id)
     (match (syntax-local-value #'typename (λ () #f))
       [(sumtype-info variants)
        (with-syntax ([(varname ...) (map symbol->string (map syntax->datum variants))]
                      [(variant ...) variants])
          #'(∨ (>> (literal varname) (unit (variant))) ...) )]
       [_ (raise-syntax-error #f "not a sumtype" #'typename)])]))

(define transaction-field
  (enumtype-parser i:TransactionField))

(define global-field
  (enumtype-parser i:GlobalField))

(define label-identifier
  (>>= (∘ (cc (^^ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-"))
          (p* (cc (^^ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-")))) 
       (lift (λ (c cs) (string->symbol (apply string c cs))))))

(define guarded-label
  (report label-identifier "expected a label but got ~s"))

(define asset-holding-field
  (enumtype-parser i:AssetHoldingField))

(define asset-params-field
  (enumtype-parser i:AssetParamsField))

(define app-params-field
  (enumtype-parser i:AppParamsField))

(define-sumtype Pseudoinstruction
  i:Instruction
  (int uint)
  (byte bytes))

(define-syntax instruction-parser
  (syntax-parser
    [_
     (match-let ([(sumtype-info variants) (syntax-local-value #'Pseudoinstruction)])
       (with-syntax ([(parser ...)
                      (map
                       (λ (variant)
                         (match-let ([(record-info fields _ constructor _ _ _) (syntax-local-value variant)])
                           (with-syntax ([name (symbol->string (syntax->datum variant))]
                                         [constructor constructor]
                                         [(field ...) (map
                                                        (λ (field)
                                                          (match field
                                                            ['v #'uint8]
                                                            ['i #'uint8]
                                                            ['n #'uint8]
                                                            ['uints #'(p* (>> whitespace* varuint))]
                                                            ['bytess #'(p* (>> whitespace* pbytes))]
                                                            ['group-index #'uint8]
                                                            ['array-index #'uint8]
                                                            ['offset #'guarded-label]
                                                            ['start #'uint8]
                                                            ['end #'uint8]
                                                            ['length #'uint8]
                                                            ['bytes #'pbytes]
                                                            ['uint #'varuint]
                                                            ['field
                                                             (match (syntax->datum variant)
                                                               [(or 'txn 'gtxn 'txna 'gtxna 'gtxns 'gtxnsa 'itxn_field 'itxn 'itxna 'txnas 'gtxnas 'gtxnsas)
                                                                #'transaction-field]
                                                               ['global #'global-field]
                                                               ['asset_holding_get #'asset-holding-field]
                                                               ['asset_params_get #'asset-params-field]
                                                               ['app_params_get #'app-params-field])]
                                                            ))
                                                        fields)])
                             #'(make-instruction name constructor field ...))))
                       variants)])
         #'(∨ parser ...)))]))

(define instruction
  (>> whitespace*
      (instruction-parser)))

(define newline
  (>>= read-char
       (λ (c)
         (if (eqv? c #\newline)
           (unit #f)
           (if (eqv? c #\return)
             (>>= read-char
                  (λ (c)
                    (if (eqv? c #\newline)
                      (unit #f)
                      fail)))
             fail)))))

(record label (ℓ))

(define label-declaration
  (>>= label-identifier
       (λ (ℓ)
         (>> space*
             (literal ":")
             (unit (label ℓ))))))

(define comment
  (>> (literal "//")
      (>>= (p* (cc (^ "\r\n"))) (lift (λ (cs) (apply string cs))))))

(define (maybe-comment-after v)
  (>> space*
      (p? comment)
      (∨ newline end-of-input)
      (unit v)))

(define line
  (>>= (p? (∨ pragma-directive
              instruction
              label-declaration))
       (λ (v)
         (>> whitespace*
             (p? comment)
             (∨ newline end-of-input)
             (unit v)))))

(define (parse input)
  (let loop ([i 0])
    (end-of-input input i (λ (_ i fk) (list))
                  (λ ()
                    (line input i (λ (ds i fk)
                                    (match ds
                                      [(list directive)
                                       (if directive
                                         (cons directive (loop i))
                                         (loop i))]
                                      [_
                                       (error 'parser "expected single result")]))
                          (λ ()
                            (error 'parse "uncaught error with ~s" (snippet input i))))))))

(module+ main
  (require racket/port
           racket/pretty)
  (let ([input (port->string (current-input-port))])
    (let ([instructions (time (parse input))])
      (pretty-print instructions)
      #;
      (let ([initial-ph (make-placeholder #f)])
        (let ([phs (let loop ([instructions instructions]
                              [ph initial-ph]
                              [phs (hasheq)])
                     (match instructions
                       [(list)
                        (placeholder-set! ph (list))
                        phs]
                       [(cons instr instructions)
                        (let* ([next-ph (make-placeholder #f)]
                               [phs (match instr
                                      [`(label ,ℓ)
                                       (placeholder-set! ph next-ph)
                                       (hash-set phs ℓ next-ph)]
                                      [_
                                       (placeholder-set! ph (cons instr next-ph))
                                       phs])])
                          (loop instructions next-ph phs))]))])
          (let loop ([ph initial-ph])
            (match (placeholder-get ph)
              [(? placeholder? ph)
               (loop ph)]
              [(list)
               (void)]
              [(cons instr next-ph)
               (match instr
                 [(list (and code (or 'bnz 'bz 'callsub)) ℓ)
                  (cond
                    [(hash-ref phs ℓ #f)
                     => (λ (is-ph)
                          (placeholder-set! ph (cons (list code is-ph) next-ph)))]
                    [else
                     (error 'parse "unknown label ~a" ℓ)])]
                 [`(b ,ℓ)
                  (cond
                    [(hash-ref phs ℓ #f)
                     => (λ (is-ph)
                          (placeholder-set! ph is-ph))]
                    [else
                     (error 'parse "unknown label ~a" ℓ)])]
                 [(or `(err)
                      `(return)
                      `(retsub))
                  (placeholder-set! ph (cons instr (list)))]
                 [_
                  (void)])
               (loop next-ph)]))
          (pretty-print (make-reader-graph initial-ph)))))))
