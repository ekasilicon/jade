#lang racket/base
(require racket/match)

; all instructions begin with a byte
; some instructions, such as bz, are followed by a static (with respect to the instruction byte) number of bytes
; some, such as intcblock, are followed by a dynamic number of bytes (determined by the bytes that follow it)

; the Go VM records instructions previously visited and ensures that backward jumps are
; instruction-aligned.
; such an approach is probably better because we could have a byte sequence which is
; always skipped which is not a valid encoding of instructions.
; therefore, we should assume that the target of a forward jump (not before seen)
; is on an instruction boundary

(define layout
  (hasheqv #x20 ; intcblock
           '((times varuint varuint)) 
           #x21 ; intc
           '(uint8)
           #x26 ; bytecblock
           '((times varuint (times varuint byte)))
           #x27 ; bytec
           '(uint8)
           #x2c ; arg
           '(uint8)
           #x31 ; txn
           '(uint8)
           #x32 ; global
           '(uint8)
           #x33 ; gtxn
           '(uint8 uint8)
           #x34 ; load
           '(uint8)
           #x35 ; store
           '(uint8)
           #x36 ; txna
           '(uint8 uint8)
           #x37 ; gtxna
           '(uint8 uint8 uint8)
           #x38 ; gtxns
           '(uint8)
           #x39 ; gtxnsa
           '(uint8 uint8)
           #x3a ; gload
           '(uint8 uint8)
           #x3b ; gloads
           '(uint8)
           #x3c ; gaid
           '(uint8)
           #x40 ; bnz
           '(int16)
           #x41 ; bz
           '(int16)
           #x42 ; b
           '(int16)
           #x4b ; dig
           '(uint8)
           #x51 ; substring
           '(uint8 uint8)
           #x57 ; extract
           '(uint8 uint8)
           #x70 ; asset_holding_get
           '(uint8)
           #x71 ; asset_params_get
           '(uint8)
           #x72 ; app_params_get
           '(uint8)
           #x80 ; pushbytes
           '((times varuint byte))
           #x81 ; pushint
           '(varuint)
           #x88 ; callsub
           '(int16)))
