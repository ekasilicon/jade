#lang racket/base
(require racket/match
         racket/string
         racket/pretty)

(define ((unit . xs) input i) `(success ,xs ,i))
(define (fail input i) #f)

(define ((>>= m f) input i)
  (match (m input i)
    [`(success ,xs ,i)
     ((apply f xs) input i)]
    [`(failure ,msg)
     `(failure ,msg)]
    [#f
     #f]))

(define (>> m . ms)
  (foldl (λ (m m₀) (>>= m₀ (λ _ m))) m ms))

(define ((lift f) . xs)
  (call-with-values (λ () (apply f xs)) unit))

(define ((∨ . ps) input i) 
  (let loop ([ps ps])
    (match ps
      [(list)
       (fail input i)]
      [(cons p ps)
       (match (p input i)
         [`(success ,xs ,i)
          `(success ,xs ,i)]
         [`(failure ,msg)
          (error 'parse "alternate returns a failure with message ~s" msg)]
         [#f
          (loop ps)])])))

(define (∘ . ps)
  (let loop ([ps ps])
    (match ps
      [(list)
       (unit)]
      [(cons p ps)
       (>>= p (λ (x) (>>= (loop ps) (λ xs (apply unit x xs)))))])))

(define ((p* p) input i) 
  (let loop ([i i]
             [xs (list)])
    (match (p input i)
      [`(success ,(list x) ,i)
       (loop i (cons x xs))]
      [`(failure ,msg)
       (error 'parse "repetition has a failure of its own with message ~s" msg)]
      [#f
       `(success ,(list (reverse xs)) ,i)])))

(define (p+ p)
  (>>= p (λ (x) (>>= (p* p) (λ (xs) (unit (cons x xs)))))))

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

(define (end-of-input input i)
  (if (= (string-length input) i)
    ((unit #f) input i) 
    (fail input i)))

(define (read-char input i)
  (if (= (string-length input) i)
    #f
    `(success ,(list (string-ref input i)) ,(add1 i))))

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

(define (snippet input i)
  (substring input i (min (string-length input) (+ i 64))))

(define ((report p template) input i)
  (match (p input i)
    [`(success ,xs ,i)
     `(success ,xs ,i)]
    [`(failure ,msg)
     `(failure ,msg)]
    [#f
     `(failure ,(format template (snippet input i)))]))

(define ((trace which p) input i)
  (displayln 'TRACE)
  (displayln which)
  (println p)
  (println i)
  (println (snippet input i))
  (match (p input i)
    [`(success ,xs ,i)
     (println `(success ,xs ,i))
     `(success ,xs ,i)]))

(define ((not-implemented id) input i)
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

(define pbytes
  (∨ (>> (literal "base64")
         space+
         (>>= (p* (cc (^ "\r\n")))
              (lift (λ (cs) (apply string cs)))))))

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


(define pragma
  (>> (literal "#pragma ")
      (>>= (p* (cc (^ "\r\n")))
           (lift (λ (cs) `(pragma ,(apply string cs)))))))

(define (make-instruction name . arguments)
  (>>= (make-symbol name)
       (λ (id)
         (>>= (let loop ([args arguments])
                (match args
                  [(list)
                   (unit (list))]
                  [(cons arg args)
                   (>> space+
                       (>>= arg
                            (λ (x)
                              (>>= (loop args)
                                   (λ (xs) (unit (cons x xs)))))))]))
              (λ (xs) (unit `(,id . ,xs)))))))

(define arithmetic-logic-cryptographic-instruction
  (apply ∨
   (map make-instruction (string-split #<<EOF
sha256
keccak256
sha512_256
ed25519verify
+
-
/
*
<=
>=
<
>
&&
||
shl
shr
sqrt
bitlen
exp
==
!=
!
len
itob
btoi
%
|
&
^
~
mulw
addw
divmodw
expw
getbit
setbit
getbyte
setbyte
concat
EOF
                                               ))))

(define byte-array-extract-instruction
  (∨ (make-instruction "substring" uint8 uint8)
     (make-instruction "substring3")
     (make-instruction "extract" uint8 uint8)
     (make-instruction "extract3")
     (make-instruction "extract16bits")
     (make-instruction "extract32bits")
     (make-instruction "extract64bits")))

(define byte-array-arithmetic-instruction
  (∨ (make-instruction "b+")
     (make-instruction "b-")
     (make-instruction "b/")
     (make-instruction "b*")
     (make-instruction "b<")
     (make-instruction "b>")
     (make-instruction "b<=")
     (make-instruction "b>=")
     (make-instruction "b==")
     (make-instruction "b!=")
     (make-instruction "b%")))

(define byte-array-logic-instruction
  (∨ (make-instruction "b|")
     (make-instruction "b&")
     (make-instruction "b^")
     (make-instruction "b~")))

(define transaction-field
  (apply ∨ (map make-symbol (string-split
                             #<<EOF
Sender
Fee
FirstValidTime
FirstValid
LastValid
Note
Lease
Receiver
Amount
CloseRemainderTo
VotePK
SelectionPK
VoteFirst
VoteLast
VoteKeyDilution
TypeEnum
Type
XferAsset
AssetAmount
AssetSender
AssetReceiver
AssetCloseTo
GroupIndex
TxID
ApplicationID
OnCompletion
ApplicationArgs
NumAppArgs
Accounts
NumAccounts
ApprovalProgram
ClearStateProgram
RekeyTo
ConfigAssetTotal
ConfigAssetDecimals
ConfigAssetDefaultFrozen
ConfigAssetUnitName
ConfigAssetName
ConfigAssetURL
ConfigAssetMetadataHash
ConfigAssetManager
ConfigAssetReserve
ConfigAssetFreeze
ConfigAssetClawback
ConfigAsset
FreezeAssetAccount
FreezeAssetFrozen
FreezeAsset
Assets
NumAssets
Applications
NumApplications
GlobalNumUint
GlobalNumByteSlice
LocalNumUint
LocalNumByteSlice
ExtraProgramPages
Nonparticipation
EOF
                             ))))

(define inner-transaction-instruction
  (∨ (make-instruction "tx_begin")
     (make-instruction "tx_field" transaction-field)
     (make-instruction "tx_submit")))

(define global-field
  (apply ∨ (map make-symbol (string-split
                             #<<EOF
MinTxnFee
MinBalance
MaxTxnLife
ZeroAddress
GroupSize
LogicSigVersion
Round
LatestTimestamp
CurrentApplicationID
CreatorAddress
CurrentApplicationAddress
GroupID
EOF
                             ))))

(define value-loading-instruction
  (∨ (>> (literal "intcblock")
         (>>= (p* (>> space+ varuint))
              (λ (varuints) `(intcblock . ,varuints))))
     (make-instruction "intc" varuint)
     (make-instruction "intc_0")
     (make-instruction "intc_1")
     (make-instruction "intc_2")
     (make-instruction "intc_3")
     (make-instruction "int" varuint) ; pseudo instruction
     (make-instruction "pushint" varuint)
     (>> (literal "bytecblock")
         (>>= (p* (>> space+ pbytes))
              (λ (bytess) `(bytecblock . ,bytess))))
     (make-instruction "bytec" varuint)
     (make-instruction "bytec_0")
     (make-instruction "bytec_1")
     (make-instruction "bytec_2")
     (make-instruction "bytec_3")
     (make-instruction "byte" pbytes) ; pseudo instruction
     (make-instruction "pushbytes" pbytes)
     (make-instruction "bzero")
     (make-instruction "arg" uint8)
     (make-instruction "arg_0")
     (make-instruction "arg_1")
     (make-instruction "arg_2")
     (make-instruction "arg_3")
     (make-instruction "txn" transaction-field)
     (make-instruction "gtxn" transaction-field)
     (make-instruction "txna" transaction-field uint8)
     (make-instruction "gtxna" uint8 transaction-field uint8)
     (make-instruction "gtxnas" uint8 transaction-field)
     (make-instruction "gtxns" (trace 'this transaction-field))
     (make-instruction "gtxnsa" transaction-field uint8)
     (make-instruction "gtxnsas" transaction-field)
     (make-instruction "global" global-field)
     (make-instruction "load" uint8)
     (make-instruction "loads")
     (make-instruction "store" uint8)
     (make-instruction "stores")
     (make-instruction "gload" uint8 uint8)
     (make-instruction "gloads" uint8)
     (make-instruction "gaid" uint8)
     (make-instruction "gaids")
     (make-instruction "args")))

(define label
  (>>= (∘ (cc (^^ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-"))
          (p* (cc (^^ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-")))) 
       (lift (λ (c cs) (string->symbol (apply string c cs))))))

(define guarded-label
  (report label "expected a label but got ~s"))

(define flow-control-instruction
  (∨ (make-instruction "err")
     (make-instruction "bnz" guarded-label)
     (make-instruction "bz" guarded-label)
     (make-instruction "b" guarded-label)
     (make-instruction "return")
     (make-instruction "pop")
     (make-instruction "dup")
     (make-instruction "dup2")
     (make-instruction "dig" uint8)
     (make-instruction "cover" uint8)
     (make-instruction "uncover" uint8)
     (make-instruction "swap")
     (make-instruction "select")
     (make-instruction "assert")
     (make-instruction "callsub" guarded-label)
     (make-instruction "retsub")))

(define asset-holding-field
  (∨ (make-symbol "AssetBalance")
     (make-symbol "AssetFrozen")))

(define asset-param-field
  (apply ∨ (map make-symbol (string-split
                             #<<ASSET-PARAM-FIELD
AssetTotal
AssetDecimals
AssetDefaultFrozen
AssetUnitName
AssetName
AssetURL
AssetMetadataHash
AssetManager
AssetReserve
AssetFreeze
AssetClawback
AssetCreator
ASSET-PARAM-FIELD
                             ))))

(define app-field
  (apply ∨ (map make-symbol (string-split
                             #<<APP-FIELD
AppApprovalProgram
AppClearStateProgram
AppGlobalNumUint
AppGlobalNumByteSlice
AppLocalNumUint
AppLocalNumByteSlice
AppExtraProgramPages
AppCreator
AppAddress
APP-FIELD
                             ))))

(define state-access-instruction
  (∨ (make-instruction "balance")
     (make-instruction "min_balance")
     (make-instruction "app_opted_in")
     (make-instruction "app_local_get")
     (make-instruction "app_local_get_ex")
     (make-instruction "app_global_get")
     (make-instruction "app_global_get_ex")
     (make-instruction "app_local_put")
     (make-instruction "app_global_put")
     (make-instruction "app_local_del")
     (make-instruction "app_global_del")
     (make-instruction "asset_holding_get" asset-holding-field)
     (make-instruction "asset_params_get" asset-param-field)
     (make-instruction "app_params_get" app-field)
     (make-instruction "log")))


(define payload
  (∨ arithmetic-logic-cryptographic-instruction
     byte-array-extract-instruction
     byte-array-arithmetic-instruction
     byte-array-logic-instruction
     inner-transaction-instruction
     value-loading-instruction
     flow-control-instruction
     state-access-instruction))

(define instruction
  (>>= (∘ (∨ (literal "\t")
             space*)
          payload
          space*)
       (lift (λ (_₀ x _₁) x))))

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

(define label-declaration
  (>>= label
       (λ (ℓ)
         (>> space*
             (literal ":")
             (unit `(label ,ℓ))))))

(define line
  (>>= (∘ (report (∨ pragma
                     instruction
                     label-declaration)
                  "expected pragma, label, or instruction at ~s")
          (∨ newline
             end-of-input))
       (lift (λ (x _) x))))

(define (parse input)
  (let loop ([i 0])
    (cond
      [(end-of-input input i)
       =>
       (match-lambda
         [`(success (,_) ,i)
          (list)]
         [`(failure ,msg)
          (error 'parse msg)])]
      [(line input i)
       =>
       (match-lambda
         [`(success (,directive) ,i)
          (cons directive (loop i))]
         [`(failure ,msg)
          (error 'parse msg)])]
      [else
       (error 'parse "uncaught error with ~s" (snippet input i))])))

(module+ test
  (parse
   #<<EOF
#pragma hello
EOF
 )

  (parse
   #<<EOF
#pragma hello
   +
EOF
 )

  (parse
   #<<EOF
#pragma hello
   +
   -
   /
   *
EOF
 ))


(module+ main
  (require racket/port
           racket/pretty)
  (let ([input (port->string (current-input-port))])
    (pretty-print (time (parse input)))))


#|

label
[+^{ \t\r\n}]
|#


#|
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
    (map
     println
     (let loop ([i 0])
      (dispatch (hash-ref nts start) i
                (λ (x i)
                  (define (newline i)
                    (if (and (not (= (string-length input) i))
                             (eqv? (string-ref input i) #\newline))
                      (let ([i (add1 i)])
                        (if (= (string-length input) i)
                          (list x)
                          (cons x (loop i))))
                      (error 'interpret "expected newline")))
                  (if (= (string-length input) i)
                    (list x)
                    (if (eqv? (string-ref input i) #\return)
                      (newline (add1 i))
                      (newline i))))
                (λ ()
                  (error 'interpret "failed to continue at ~s" (substring input i))))))))

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
pushint 0x123
pushbytes byte 0x1234
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


; apply the parser result of p to f
#;
(define (lift p f)
  (>>= p f))

(define bytes
  ...)

(define varuint
  ...)

(define )

line
(pragma)
(comment)
(instruction)
(target)

pragma
#pragma[+ ][+^{\r\n}]

comment
//[*^{\r\n}]

instruction
(arithmetic-logic-cryptographic-instruction)
(byte-array-extract-instruction)
(byte-array-arithmetic-instruction)
(byte-array-logic-instruction)
(inner-transaction-instruction) 
(value-loading-instruction)
(flow-control-instruction)
(state-access-instruction)

target
(label)[* ]:


|#
