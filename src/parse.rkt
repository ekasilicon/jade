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
          `(failure ,msg)
          #;
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

(define (p? p) (∨ p (unit #f)))

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

(require file/sha1)

(define pbytes
  (∨ (>> (literal "base64")
         space+
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
     (make-instruction "gtxns" transaction-field)
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
  (∨ (make-symbol "AssetTotal")
     (make-symbol "AssetDecimals")
     (make-symbol "AssetDefaultFrozen")
     (make-symbol "AssetUnitName")
     (make-symbol "AssetName")
     (make-symbol "AssetURL")
     (make-symbol "AssetMetadataHash")
     (make-symbol "AssetManager")
     (make-symbol "AssetReserve")
     (make-symbol "AssetFreeze")
     (make-symbol "AssetClawback")
     (make-symbol "AssetCreator")))

(define app-field
  (∨ (make-symbol "AppApprovalProgram")
     (make-symbol "AppClearStateProgram")
     (make-symbol "AppGlobalNumUint")
     (make-symbol "AppGlobalNumByteSlice")
     (make-symbol "AppLocalNumUint")
     (make-symbol "AppLocalNumByteSlice")
     (make-symbol "AppExtraProgramPages")
     (make-symbol "AppCreator")
     (make-symbol "AppAddress")))

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


(define instruction
  (>> (∨ (literal "\t")
         space*)
      (∨ arithmetic-logic-cryptographic-instruction
         byte-array-extract-instruction
         byte-array-arithmetic-instruction
         byte-array-logic-instruction
         inner-transaction-instruction
         value-loading-instruction
         flow-control-instruction
         state-access-instruction)))

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

(define comment
  (>> (literal "//")
      (>>= (p* (cc (^ "\r\n"))) (lift (λ (cs) (apply string cs))))))

(define (maybe-comment-after v)
  (>> space*
      (p? comment)
      (∨ newline end-of-input)
      (unit v)))

(define line
  ; we must distinguish between a comment on a blank line
  ; and a comment following context because the division instruction
  ; / is a prefix of the comment indicator //
  (∨ (maybe-comment-after #f)
     (>>= (report (∨ pragma
                     instruction
                     label-declaration)
                  "expected pragma, label, or instruction at ~s")
          maybe-comment-after)))

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
          (if directive
            (cons directive (loop i))
            (loop i))]
         [`(failure ,msg)
          (error 'parse msg)])]
      [else
       (error 'parse "uncaught error with ~s" (snippet input i))])))

(module+ main
  (require racket/port
           racket/pretty)
  (let ([input (port->string (current-input-port))])
    (let ([instructions (time (parse input))])
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
                                      [`(or `(err) `(return))
                                       (placeholder-set! ph (cons instr (list)))]
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
                 [(list (and code (or 'bnz 'bz 'b 'callsub)) ℓ)
                  (cond
                    [(hash-ref phs ℓ #f)
                     => (λ (is-ph)
                          (placeholder-set! ph (cons (list code is-ph) next-ph)))]
                    [else
                     (error 'parse "unknown label ~a" ℓ)])]
                 [_
                  (void)])
               (loop next-ph)]))
          (pretty-print (make-reader-graph initial-ph)))))))

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

