#lang racket/base
(require racket/match
         "monad.rkt"
         "read-byte.rkt")

(define transaction-field
  (match-lambda
    [0  'Sender]
    [1  'Fee]
    [6  'Lease]
    [7  'Receiver]
    [8  'Amount]
    [9  'CloseRemainderTo]
    [16 'TypeEnum]
    [17 'XferAsset]
    [18 'AssetAmount]
    [20 'AssetReceiver]
    [21 'AssetCloseTo]
    [22 'GroupIndex]
    [24 'ApplicationID]
    [25 'OnCompletion]
    [26 'ApplicationArgs]
    [27 'NumAppArgs]
    [29 'NumAccounts]
    [32 'RekeyTo]))

(define (read-transaction-field rb)
  (match-define (ReadByte [Monad (Monad unit >>=)]) rb)
  (>>= (read-uint8 rb) 
       (λ (i) (unit (transaction-field i)))))

(define global-field
  (match-lambda
    [3  'ZeroAddress]
    [4  'GroupSize]
    [6  'Round]
    [7  'LatestTimestamp]
    [9  'CreatorAddress]))

(define (read-global-field rb)
  (match-define (ReadByte [Monad (Monad unit >>=)]) rb)
  (>>= (read-uint8 rb)
       (λ (i) (unit (global-field i)))))

(define read-offset read-int16)

; ReadByte => m Instruction
(define (read-instruction rb)
  (match-define (ReadByte [Monad (Monad unit >>=)]) rb)
  (>>= (read-opcode rb) 
       (match-lambda
         [#x00 (unit `(err))]
         [#x01 (unit `(sha256))]
         [#x02 (unit `(keccak256))]
         [#x03 (unit `(sha512_256))]
         [#x04 (unit `(ed25519verify))]
         [#x05 (>>= (read-uint8 rb) (λ (v) (unit `(ecdsa_verify ,v))))]
         [#x06 (>>= (read-uint8 rb) (λ (v) (unit `(ecdsa_pk_decompress ,v))))]
         [#x07 (>>= (read-uint8 rb) (λ (v) (unit `(ecdsa_pk_recover ,v))))]
         [#x08 (unit `(+))]
         [#x09 (unit `(-))]
         [#x0a (unit `(/))]
         [#x0b (unit `(*))]
         [#x0c (unit `(<))]
         [#x0d (unit `(>))]
         [#x0e (unit `(<=))]
         [#x0f (unit `(>=))]
         [#x10 (unit `(&&))]
         [#x11 (unit `(\|\|))]
         [#x12 (unit `(==))]
         [#x13 (unit `(!=))]
         [#x14 (unit `(!))]
         [#x15 (unit `(len))]
         [#x16 (unit `(itob))]
         [#x17 (unit `(btoi))]
         [#x18 (unit `(%))]
         [#x1a (unit `(&))]
         [#x20 (>>= (read-intcblock rb) (λ (ns) (unit `(intcblock . ,ns))))]
         [#x21 (>>= (read-uint8 rb) (λ (i) (unit `(intc ,i))))]
         [#x22 (unit `(intc_0))]
         [#x23 (unit `(intc_1))]
         [#x24 (unit `(intc_2))]
         [#x25 (unit `(intc_3))]
         [#x26 (>>= (read-bytecblock rb) (λ (bss) (unit `(bytecblock . ,bss))))]
         [#x27 (>>= (read-uint8 rb) (λ (i) (unit `(bytec ,i))))]
         [#x28 (unit `(bytec_0))]
         [#x29 (unit `(bytec_1))]
         [#x2a (unit `(bytec_2))]
         [#x2b (unit `(bytec_3))]
         [#x31 (>>= (read-transaction-field rb) (λ (f) (unit `(txn ,f))))]
         [#x32 (>>= (read-global-field rb) (λ (f) (unit `(global ,f))))]
         [#x33 (>>= (read-uint8 rb) (λ (gi) (>>= (read-transaction-field rb) (λ (f) (unit `(gtxn ,gi ,f))))))]
         [#x34 (>>= (read-uint8 rb) (λ (i) (unit `(load ,i))))]
         [#x35 (>>= (read-uint8 rb) (λ (i) (unit `(store ,i))))]
         [#x36 (>>= (read-transaction-field rb) (λ (f) (>>= (read-uint8 rb) (λ (ai) (unit `(txna ,f ,ai))))))]
         [#x37 (>>= (read-uint8 rb) (λ (gi) (>>= (read-transaction-field rb) (λ (f) (>>= (read-uint8 rb) (λ (ai) (unit `(gtxna ,gi ,f ,ai))))))))]
         [#x38 (>>= (read-transaction-field rb)  (λ (f) (unit `(gtxns ,f))))]
         [#x40 (>>= (read-offset rb) (λ (offset) (unit `(bnz ,offset))))]
         [#x41 (>>= (read-offset rb) (λ (offset) (unit `(bz ,offset))))]
         [#x42 (>>= (read-offset rb) (λ (offset) (unit `(b ,offset))))]
         [#x43 (unit `(return))]
         [#x44 (unit `(assert))]
         ; #x45-#x47 unused
         [#x48 (unit `(pop))]
         [#x49 (unit `(dup))]
         [#x4a (unit `(dup2))]
         [#x4b (>>= (read-uint8 rb) (λ (n) (unit `(dig ,n))))]
         [#x4c (unit `(swap))]
         [#x4d (unit `(select))]
         [#x4e (>>= (read-uint8 rb) (λ (n) (unit `(cover ,n))))]
         [#x4f (>>= (read-uint8 rb) (λ (n) (unit `(uncover ,n))))]
         [#x50 (unit `(concat))]
         [#x51 (>>= (read-uint8 rb) (λ (s) (>>= (read-uint8 rb) (λ (e) (unit `(substring ,s ,e))))))]
         [#x52 (unit `(substring3))]
         [#x55 (unit `(getbyte))]
         [#x56 (unit `(setbyte))]
         [#x62 (unit `(app_local_get))]
         [#x64 (unit `(app_global_get))]
         [#x65 (unit `(app_global_get_ex))]
         [#x66 (unit `(app_local_put))]
         [#x67 (unit `(app_global_put))]
         [#x68 (unit `(app_local_del))]
         [#x69 (unit `(app_global_del))]
         [#x80 (>>= (read-bytes rb) (λ (bs) (unit `(pushbytes ,bs))))]
         [#x81 (>>= (read-varuint rb) (λ (n) (unit `(pushint ,n))))]
         [#x88 (>>= (read-offset rb) (λ (offset) (unit `(callsub ,offset))))]
         [#x89 (unit `(retsub))]
         [#xaa (unit `(b%))]
         [#xab (unit `(b\|))]
         [#xac (unit `(b&))]
         [#xad (unit `(b^))]
         [#xae (unit `(b~))]
         [#xaf (unit `(bzero))]
         [#xb0 (unit `(log))]
         [#xb1 (unit `(itxn_begin))]
         [#xb2 (>>= transaction-field (λ (f) (unit `(itxn_field ,f))))]
         [#xb3 (unit `(itxn_submit))]
         [#xb4 (>>= transaction-field (λ (f) (unit `(itxn ,f))))]
         [#xb5 (>>= transaction-field (λ (f) (>>= (read-uint8 rb)  (λ (i) (unit `(itxna ,f ,i))))))]
         ; #xb6-#xbf unused
         [#xc0 (>>= transaction-field (λ (f) (unit `(txnas ,f))))])))

(provide read-instruction)
