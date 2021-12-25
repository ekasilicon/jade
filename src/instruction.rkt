#lang racket/base
(require (prefix-in r: (only-in racket/base <=))
         (only-in racket/list take)
         racket/match
         "static/sumtype.rkt"
         "static/object.rkt"
         #;"static/sumtype-extra.rkt"
         "monad.rkt"
         "read-byte.rkt"
         #;
         (rename-in "logic-sig-version.rkt" [LogicSigVersion LSV]))

(define instr0
  (inc (logic-sig-version
        >>=)
       [decode-instruction
        (λ (oc)
          (>>= logic-sig-version
               (λ (lsv)
                 (error 'read-instruction
                        "illegal opcode ~a for TEAL version ~a"
                        (number->string oc 16)
                        lsv))))]))

(define-sumtype TransactionField1
  (Sender)
  (Fee)
  (FirstValid)
  (FirstValidTime)
  (LastValid)
  (Note)
  (Lease)
  (Receiver)
  (Amount)
  (CloseRemainderTo)
  (VotePK)
  (SelectionPK)
  (VoteFirst)
  (VoteLast)
  (VoteKeyDilution)
  (Type)
  (TypeEnum)
  (XferAsset)
  (AssetAmount)
  (AssetSender)
  (AssetReceiver)
  (AssetCloseTo)
  (GroupIndex))

(define-sumtype GlobalField1
  (MinTxnFee)
  (MinBalance)
  (MaxTxnLife)
  (ZeroAddress)
  (GroupSize))

(define-sumtype Instruction1
  (err)
  (sha256)
  (keccak256)
  (sha512_256)
  (ed25519verify)
  (+)
  (-)
  (/)
  (*)
  (<)
  (>)
  (<=)
  (>=)
  (&&)
  (\|\|)
  (==)
  (!=)
  (!)
  (len)
  (itob)
  (btoi)
  (%)
  (\|)
  (&)
  (^)
  (~)
  (mulw)
  (intcblock uints)
  (intc i)
  (intc_0)
  (intc_1)
  (intc_2)
  (intc_3)
  (bytecblock bytess)
  (bytec i)
  (bytec_0)
  (bytec_1)
  (bytec_2)
  (bytec_3)
  (arg n)
  (arg_0)
  (arg_1)
  (arg_2)
  (arg_3)
  (txn field)
  (global field)
  (gtxn group-index field)
  (load i)
  (store i)
  (bnz offset)
  (pop)
  (dup))

(define instr1
  (inc (read-opcode read-uint8 read-offset read-intcblock read-bytecblock
        unit >>=)
       [instruction-logic-signature-version
        (sumtype-case-lambda Instruction1
          #:otherwise (λ (_) 1))]
       [instruction-name
        (sumtype-case-lambda Instruction1
          #:otherwise (λ (_) "Instruction1"))]
       [read-instruction
        (>>= read-opcode decode-instruction)]
       [decode-instruction
        (match-lambda
          [#x00 (unit (err))]
          [#x01 (unit (sha256))]
          [#x02 (unit (keccak256))]
          [#x03 (unit (sha512_256))]
          [#x04 (unit (ed25519verify))]
          [#x08 (unit (+))]
          [#x09 (unit (-))]
          [#x0a (unit (/))]
          [#x0b (unit (*))]
          [#x0c (unit (<))]
          [#x0d (unit (>))]
          [#x0e (unit (<=))]
          [#x0f (unit (>=))]
          [#x10 (unit (&&))]
          [#x11 (unit (\|\|))]
          [#x12 (unit (==))]
          [#x13 (unit (!=))]
          [#x14 (unit (!))]
          [#x15 (unit (len))]
          [#x16 (unit (itob))]
          [#x17 (unit (btoi))]
          [#x18 (unit (%))]
          [#x19 (unit (\|))]
          [#x1a (unit (&))]
          [#x1b (unit (^))]
          [#x1c (unit (~))]
          [#x1d (unit (mulw))]
          [#x20 (>>= read-intcblock (λ (uints) (unit (intcblock uints))))]
          [#x21 (>>= read-uint8 (λ (i) (unit (intc i))))]
          [#x22 (unit (intc_0))]
          [#x23 (unit (intc_1))]
          [#x24 (unit (intc_2))]
          [#x25 (unit (intc_3))]
          [#x26 (>>= read-bytecblock (λ (bytess) (unit (bytecblock bytess))))]
          [#x27 (>>= read-uint8 (λ (i) (unit (bytec i))))]
          [#x28 (unit (bytec_0))]
          [#x29 (unit (bytec_1))]
          [#x2a (unit (bytec_2))]
          [#x2b (unit (bytec_3))]
          [#x2c (>>= read-uint8 (λ (n) (unit (arg n))))]
          [#x2d (unit (arg_0))]
          [#x2e (unit (arg_1))]
          [#x2f (unit (arg_2))]
          [#x30 (unit (arg_3))]
          [#x31 (>>= read-transaction-field (λ (field) (unit (txn field))))]
          [#x32 (>>= read-global-field (λ (f) (unit (global [field f]))))]
          [#x33 (>>= read-uint8 (λ (group-index) (>>= read-transaction-field (λ (field) (unit (gtxn group-index field))))))]
          [#x34 (>>= read-uint8 (λ (i) (unit (load i))))]
          [#x35 (>>= read-uint8 (λ (i) (unit (store i))))]
          [#x40 (>>= read-offset (λ (offset) (unit (bnz offset))))]
          [#x48 (unit (pop))]
          [#x49 (unit (dup))]
          [oc   ((super decode-instruction) oc)])]
       [read-transaction-field
        (>>= read-uint8 decode-transaction-field)]
       [decode-transaction-field
        (match-lambda
          [0  (unit (Sender))]
          [1  (unit (Fee))]
          [2  (unit (FirstValid))]
          [3  (unit (FirstValidTime))]
          [4  (unit (LastValid))]
          [5  (unit (Note))]
          [6  (unit (Lease))]
          [7  (unit (Receiver))]
          [8  (unit (Amount))]
          [9  (unit (CloseRemainderTo))]
          [10 (unit (VotePK))]
          [11 (unit (SelectionPK))]
          [12 (unit (VoteFirst))]
          [13 (unit (VoteLast))]
          [14 (unit (VoteKeyDilution))]
          [15 (unit (Type))]
          [16 (unit (TypeEnum))]
          [17 (unit (XferAsset))]
          [18 (unit (AssetAmount))]
          [19 (unit (AssetSender))]
          [20 (unit (AssetReceiver))]
          [21 (unit (AssetCloseTo))]
          [22 (unit (GroupIndex))]
          [23 (unit (TxID))]
          [bc ((super decode-transaction-field) bc)])]
       [read-global-field
        (>>= read-uint8 decode-global-field)]
       [decode-global-field
        (match-lambda
          [0  (unit (MinTxnFee))]
          [1  (unit (MinBalance))]
          [2  (unit (MaxTxnLife))]
          [3  (unit (ZeroAddress))]
          [4  (unit (GroupSize))]
          [bc ((super decode-global-field) bc)])]))

(provide (sumtype-out Instruction1)
         (sumtype-out TransactionField1)
         (sumtype-out GlobalField1))

(define-sumtype TransactionField2
  TransactionField1
  (TxID)
  (ApplicationID)
  (OnCompletion)
  (ApplicationArgs)
  (NumAppArgs)
  (Accounts)
  (NumAccounts)
  (ApprovalProgram)
  (ClearStateProgram)
  (RekeyTo)
  (ConfigAsset)
  (ConfigAssetTotal)
  (ConfigAssetDecimals)
  (ConfigAssetDefaultFrozen)
  (ConfigAssetUnitName)
  (ConfigAssetName)
  (ConfigAssetURL)
  (ConfigAssetMetadataHash)
  (ConfigAssetManager)
  (ConfigAssetReserve)
  (ConfigAssetFreeze)
  (ConfigAssetClawback)
  (FreezeAsset)
  (FreezeAssetAccount)
  (FreezeAssetFrozen))

(define-sumtype GlobalField2
  GlobalField1
  (LogicSigVersion)
  (Round)
  (LatestTimestamp)
  (CurrentApplicationID))

(define-sumtype AssetParamsField2
  (AssetTotal)
  (AssetDecimals)
  (AssetDefaultFrozen)
  (AssetUnitName)
  (AssetName)
  (AssetURL)
  (AssetMetadataHash)
  (AssetManager)
  (AssetReserve)
  (AssetFreeze)
  (AssetClawback))

(define-sumtype AssetHoldingField2
  (AssetBalance)
  (AssetFrozen))

(define-sumtype Instruction2
  Instruction1
  (addw)
  (txna field array-index)
  (gtxna group-index field array-index)
  (bz offset)
  (b offset)
  (return)
  (dup2)
  (concat)
  (substring start end)
  (substring3)
  (balance)
  (app_opted_in)
  (app_local_get)
  (app_local_get_ex)
  (app_global_get)
  (app_global_get_ex)
  (app_local_put)
  (app_global_put)
  (app_local_del)
  (app_global_del)
  (asset_holding_get field)
  (asset_params_get field))

(define instr2
  (inc (read-transaction-field read-global-field
        read-uint8 read-offset read-bytes read-varuint
        unit >>=)
       [instruction-logic-signature-version
        (sumtype-case-lambda Instruction2
          [(Instruction1 instr) 
           ((super instruction-logic-signature-version) instr)]
          #:otherwise (λ (_) 2))]
       [instruction-name
        (sumtype-case-lambda Instruction2
          [(Instruction1 instr)
           ((super instruction-name) instr)]
          #:otherwise (λ (_) "Instruction2"))]
       [decode-instruction
        (match-lambda
          [#x1e (unit (addw))]
          [#x31 (>>= read-transaction-field (λ (field) (unit (txn field))))]
          [#x32 (>>= read-global-field (λ (field) (unit (global field))))]
          [#x33 (>>= read-uint8 (λ (group-index) (>>= read-transaction-field (λ (field) (unit (gtxn group-index field))))))]
          [#x36 (>>= read-transaction-field (λ (field) (>>= read-uint8 (λ (array-index) (unit (txna field array-index))))))]
          [#x37 (>>= read-uint8 (λ (group-index) (>>= read-transaction-field (λ (field) (>>= read-uint8 (λ (array-index) (unit (gtxna group-index field array-index))))))))]
          [#x41 (>>= read-offset (λ (offset) (unit (bz offset))))]
          [#x42 (>>= read-offset (λ (offset) (unit (b offset))))]
          [#x43 (unit (return))]
          [#x4a (unit (dup2))]
          [#x50 (unit (concat))]
          [#x51 (>>= read-uint8 (λ (start) (>>= read-uint8 (λ (end) (unit (substring start end))))))]
          [#x52 (unit (substring3))]
          [#x60 (unit (balance))]
          [#x61 (unit (app_opted_in))]
          [#x62 (unit (app_local_get))]
          [#x63 (unit (app_local_get_ex))]
          [#x64 (unit (app_global_get))]
          [#x65 (unit (app_global_get_ex))]
          [#x66 (unit (app_local_put))]
          [#x67 (unit (app_global_put))]
          [#x68 (unit (app_local_del))]
          [#x69 (unit (app_global_del))]
          [#x70 (>>= read-asset-holding-field (λ (field) (unit (asset_holding_get field))))]
          [#x71 (>>= read-asset-params-field (λ (field) (unit (asset_params_get field))))]
          [oc   ((super decode-instruction) oc)])]
       [decode-transaction-field
        (match-lambda
          [24 (unit (ApplicationID))]
          [25 (unit (OnCompletion))]
          [26 (unit (ApplicationArgs))]
          [27 (unit (NumAppArgs))]
          [28 (unit (Accounts))]
          [29 (unit (NumAccounts))]
          [30 (unit (ApprovalProgram))]
          [31 (unit (ClearStateProgram))]
          [32 (unit (RekeyTo))]
          [33 (unit (ConfigAsset))]
          [34 (unit (ConfigAssetTotal))]
          [35 (unit (ConfigAssetDecimals))]
          [36 (unit (ConfigAssetDefaultFrozen))]
          [37 (unit (ConfigAssetUnitName))]
          [38 (unit (ConfigAssetName))]
          [39 (unit (ConfigAssetURL))]
          [40 (unit (ConfigAssetMetadataHash))]
          [41 (unit (ConfigAssetManager))]
          [42 (unit (ConfigAssetReserve))]
          [43 (unit (ConfigAssetFreeze))]
          [44 (unit (ConfigAssetClawback))]
          [45 (unit (FreezeAsset))]
          [46 (unit (FreezeAssetAccount))]
          [47 (unit (FreezeAssetFrozen))]
          [bc ((super decode-transaction-field) bc)])]
       [decode-global-field
        (match-lambda
          [5  (unit (LogicSigVersion))]
          [6  (unit (Round))]
          [7  (unit (LatestTimestamp))]
          [8  (unit (CurrentApplicationID))]
          [bc ((super decode-global-field) bc)])]
       [read-asset-holding-field
        (>>= read-uint8 decode-asset-holding-field)]
       [decode-asset-holding-field
        (match-lambda
          [0 (unit (AssetBalance))]
          [1 (unit (AssetFrozen))])]
       [read-asset-params-field
        (>>= read-uint8 decode-asset-params-field)]
       [decode-asset-params-field
        (match-lambda)]))

(provide (sumtype-out Instruction2)
         (sumtype-out TransactionField2)
         (sumtype-out GlobalField2)
         (sumtype-out AssetParamsField2)
         (sumtype-out AssetHoldingField2))

(define-sumtype TransactionField3
  TransactionField2
  (Assets)
  (NumAssets)
  (Applications)
  (NumApplications)
  (GlobalNumUint)
  (GlobalNumByteSlice)
  (LocalNumUint)
  (LocalNumByteSlice))

(define-sumtype GlobalField3
  GlobalField2
  (CreatorAddress))

(define-sumtype Instruction3
  Instruction2
  (gtxns field)
  (gtxnsa field array-index)
  (assert)
  (dig n)
  (swap)
  (select)
  (getbit)
  (setbit)
  (getbyte)
  (setbyte)
  (min_balance)
  (pushbytes bytes)
  (pushint uint))

(define instr3
  (inc (read-transaction-field read-global-field
        read-uint8 read-offset read-bytes read-varuint
        unit >>=)
       [instruction-logic-signature-version
        (sumtype-case-lambda Instruction3
          [(Instruction2 instr) 
           ((super instruction-logic-signature-version) instr)]
          #:otherwise (λ (_) 3))]
       [instruction-name
        (sumtype-case-lambda Instruction3
          [(Instruction2 instr)
           ((super instruction-name) instr)]
          #:otherwise (λ (_) "Instruction3"))]
       [decode-instruction
        (match-lambda
          [#x31 (>>= read-transaction-field (λ (field) (unit (txn field))))]
          [#x32 (>>= read-global-field (λ (field) (unit (global field))))]
          [#x33 (>>= read-uint8 (λ (group-index) (>>= read-transaction-field (λ (field) (unit (gtxn group-index field))))))]
          [#x36 (>>= read-transaction-field (λ (field) (>>= read-uint8 (λ (array-index) (unit (txna field array-index))))))]
          [#x37 (>>= read-uint8 (λ (group-index) (>>= read-transaction-field (λ (field) (>>= read-uint8 (λ (array-index) (unit (gtxna group-index field array-index))))))))]
          [#x38 (>>= read-transaction-field (λ (field) (unit (gtxns field))))]
          [#x39 (>>= read-transaction-field (λ (field) (>>= read-uint8 (λ (array-index) (unit (gtxnsa field array-index))))))]
          [#x44 (unit (assert))]
          [#x4b (>>= read-uint8 (λ (n) (unit (dig n))))]
          [#x4c (unit (swap))]
          [#x4d (unit (select))]
          [#x53 (unit (getbit))]
          [#x54 (unit (setbit))]
          [#x55 (unit (getbyte))]
          [#x56 (unit (setbyte))]
          [#x57 (>>= read-uint8 (λ (start) (>>= read-uint8 (λ (length) (unit (extract start length))))))]
          [#x78 (unit (min_balance))]
          [#x80 (>>= read-bytes (λ (bytes) (unit (pushbytes bytes))))]
          [#x81 (>>= read-varuint (λ (uint) (unit (pushint uint))))]
          [oc   ((super decode-instruction) oc)])]
       [decode-transaction-field
        (match-lambda
          [48 (unit (Assets))]
          [49 (unit (NumAssets))]
          [50 (unit (Applications))]
          [51 (unit (NumApplications))]
          [52 (unit (GlobalNumUint))]
          [53 (unit (GlobalNumByteSlice))]
          [54 (unit (LocalNumUint))]
          [55 (unit (LocalNumByteSlice))]
          [bc ((super decode-transaction-field) bc)])]
       [decode-global-field
        (match-lambda
          [9  (unit (CreatorAddress))]
          [bc ((super decode-global-field) bc)])]))

(provide (sumtype-out Instruction3)
         (sumtype-out TransactionField3)
         (sumtype-out GlobalField3))

(define-sumtype TransactionField4
  TransactionField3
  (ExtraProgramPages))

(define-sumtype Instruction4
  Instruction3
  (divmodw)
  (gload group-index i)
  (gloads i)
  (gaid group-index)
  (gaids)
  (callsub offset)
  (retsub)
  (shl)
  (shr)
  (sqrt)
  (bitlen)
  (exp)
  (expw)
  (b+)
  (b-)
  (b/)
  (b*)
  (b<)
  (b>)
  (b<=)
  (b>=)
  (b==)
  (b!=)
  (b%)
  (b\|)
  (b&)
  (b^)
  (b~)
  (bzero))

(define instr4
  (inc (read-uint8 read-offset
        unit >>=)
       [instruction-logic-signature-version
        (sumtype-case-lambda Instruction4
          [(Instruction3 instr) 
           ((super instruction-logic-signature-version) instr)]
          #:otherwise (λ (_) 4))]
       [instruction-name
        (sumtype-case-lambda Instruction4
          [(Instruction3 instr)
           ((super instruction-name) instr)]
          #:otherwise (λ (_) "Instruction4"))]
       [decode-instruction
        (match-lambda
          [#x1f (unit (divmodw))]
          [#x3a (>>= read-uint8 (λ (group-index) (>>= read-uint8 (λ (i) (unit (gload group-index i))))))]
          [#x3b (>>= read-uint8 (λ (i) (unit (gloads i))))]
          [#x3c (>>= read-uint8 (λ (group-index) (unit (gaid group-index))))]
          [#x3d (unit (gaids))]
          [#x88 (>>= read-offset (λ (offset) (unit (callsub offset))))]
          [#x89 (unit (retsub))]
          [#x90 (unit (shl))]
          [#x91 (unit (shr))]
          [#x92 (unit (sqrt))]
          [#x93 (unit (bitlen))]
          [#x94 (unit (exp))]
          [#x95 (unit (expw))]
          [#xa0 (unit (b+))]
          [#xa1 (unit (b-))]
          [#xa2 (unit (b/))]
          [#xa3 (unit (b*))]
          [#xa4 (unit (b<))]
          [#xa5 (unit (b>))]
          [#xa6 (unit (b<=))]
          [#xa7 (unit (b>=))]
          [#xa8 (unit (b==))]
          [#xa9 (unit (b!=))]
          [#xaa (unit (b%))]
          [#xab (unit (b\|))]
          [#xac (unit (b&))]
          [#xad (unit (b^))]
          [#xae (unit (b~))]
          [#xaf (unit (bzero))]
          [oc   ((super decode-instruction) oc)])]
       [decode-transaction-field
        (match-lambda
          [56 (unit (ExtraProgramPages))]
          [bc ((super decode-transaction-field) bc)])]))

(provide (sumtype-out Instruction4)
         (sumtype-out TransactionField4))

(define-sumtype TransactionField5
  TransactionField4
  (Nonparticipation)
  (Logs)
  (NumLogs)
  (CreatedAssetID)
  (CreatedApplicationID))

(define-sumtype GlobalField5
  GlobalField3
  (CurrentApplicationAddress)
  (GroupID))

(define-sumtype AppParamsField5
  (AppApprovalProgram)
  (AppClearStateProgram)
  (AppGlobalNumUint)
  (AppGlobalNumByteSlice)
  (AppLocalNumUint)
  (AppLocalNumByteSlice)
  (AppExtraProgramPages)
  (AppCreator)
  (AppAddress))

(define-sumtype AssetParamsField5
  AssetParamsField2
  (AssetCreator))

(define-sumtype Instruction5
  Instruction4
  (ecdsa_verify v)
  (ecdsa_pk_decompress v)
  (ecdsa_pk_recover v)
  (loads)
  (stores)
  (cover n)
  (uncover n)
  (extract start length)
  (extract3)
  (extract_uint16)
  (extract_uint32)
  (extract_uint64)
  (app_params_get field)
  (log)
  (itxn_begin)
  (itxn_field field)
  (itxn_submit)
  (itxn field)
  (itxna field array-index)
  (txnas field)
  (gtxnas group-index field)
  (gtxnsas field)
  (args))

(define instr5
  (inc (read-transaction-field
        read-uint8
        unit >>=)
       [instruction-logic-signature-version
        (sumtype-case-lambda Instruction5
          [(Instruction4 instr)
           ((super instruction-logic-signature-version) instr)]
          #:otherwise (λ (_) 5))]
       [instruction-name
        (sumtype-case-lambda Instruction5
          [(Instruction4 instr)
           ((super instruction-name) instr)]
          #:otherwise (λ (_) "Instruction5"))]
       [decode-instruction
        (match-lambda
          [#x05 (>>= read-uint8 (λ (v) (unit (ecdsa_verify v))))]
          [#x06 (>>= read-uint8 (λ (v) (unit (ecdsa_pk_decompress v))))]
          [#x07 (>>= read-uint8 (λ (v) (unit (ecdsa_pk_recover v))))]
          [#x3e (unit (loads))]
          [#x3f (unit (stores))]
          [#x4e (>>= read-uint8 (λ (n) (unit (cover n))))]
          [#x4f (>>= read-uint8 (λ (n) (unit (uncover n))))]
          [#x57 (>>= read-uint8 (λ (start) (>>= read-uint8 (λ (length) (unit (extract start length))))))]
          [#x58 (unit (extract3))]
          [#x59 (unit (extract_uint16))]
          [#x5a (unit (extract_uint32))]
          [#x5b (unit (extract_uint64))]
          [#x72 (>>= read-app-params-field (λ (f) (unit (app_params_get [field f]))))]
          [#xb0 (unit (log))]
          [#xb1 (unit (itxn_begin))]
          [#xb2 (>>= read-transaction-field (λ (field) (unit (itxn_field field))))]
          [#xb3 (unit (itxn_submit))]
          [#xb4 (>>= read-transaction-field (λ (field) (unit (itxn field))))]
          [#xb5 (>>= read-transaction-field (λ (field) (>>= read-uint8 (λ (array-index) (unit (itxna field array-index))))))]
          [#xc0 (>>= read-transaction-field (λ (field) (unit (txnas field))))]
          [#xc1 (>>= read-uint8 (λ (group-index) (>>= read-transaction-field (λ (field) (unit (gtxnas group-index field))))))]
          [#xc2 (>>= read-transaction-field (λ (field) (unit (gtxnsas field))))]
          [#xc3 (unit (args))]
          [oc   ((super decode-instruction) oc)])]
       [decode-transaction-field
        (match-lambda
          [57 (unit (Nonparticipation))]
          [58 (unit (Logs))]
          [59 (unit (NumLogs))]
          [60 (unit (CreatedAssetID))]
          [61 (unit (CreatedApplicationID))]
          [bc ((super decode-transaction-field) bc)])]
       [decode-global-field
        (match-lambda
          [10 (unit (CurrentApplicationAddress))]
          [11 (unit (GroupID))]
          [bc ((super decode-global-field) bc)])]
       [read-app-params-field
        (>>= read-uint8 decode-app-params-field)]
       [decode-app-params-field
        (match-lambda)]))

(provide (sumtype-out Instruction5)
         (sumtype-out TransactionField5)
         (sumtype-out GlobalField5)
         (sumtype-out AppParamsField5)
         (sumtype-out AssetParamsField5))

(define-sumtype Instruction6
  Instruction5
  (itxn_next))

(define instr6
  (inc (unit)
       [instruction-logic-signature-version
        (sumtype-case-lambda Instruction6
          [(Instruction5 instr)
           ((super instruction-logic-signature-version) instr)]
          #:otherwise (λ (_) 6))]
       [instruction-name
        (sumtype-case-lambda Instruction6
          [(Instruction5 instr)
           ((super instruction-name) instr)]
          #:otherwise (λ (_) "Instruction6"))]
       [decode-instruction
        (match-lambda
          [#xb6 (unit (itxn_next))]
          [oc   ((super decode-instruction) oc)])]))

(provide (sumtype-out Instruction6))

(require "version.rkt")

(define instruction/version
  (make-*/version 'instruction/version instr0 instr1 instr2 instr3 instr4 instr5 instr6 read-byte-extras))

(provide instruction/version)

(module+ main
  (((fix (mix (instruction/version 1)
              (inc ()
                   [unit (λ xs (λ (σ) (cons xs σ)))]
                   [>>= (λ (m f)
                          (λ (σ)
                            (match-let ([(cons xs σ) (m σ)])
                              ((apply f xs) σ))))])
              (inc (unit)
                   [logic-sig-version (unit 22)])
              (inc (unit)
                   [read-byte (unit 0)])))
    'read-instruction)
   10))





#;
(define transaction-field-logic-signature-version
  (enumtype-case-lambda TransactionField
    [(Sender Fee FirstValid FirstValidTime LastValid Note Lease
      Receiver Amount CloseRemainderTo VotePK SelectionPK VoteFirst
      VoteLast VoteKeyDilution Type TypeEnum XferAsset AssetAmount
      AssetSender AssetReceiver AssetCloseTo GroupIndex TxID)
     1]
    [(ApplicationID OnCompletion ApplicationArgs NumAppArgs Accounts
      NumAccounts ApprovalProgram ClearStateProgram RekeyTo ConfigAsset
      ConfigAssetTotal ConfigAssetDecimals ConfigAssetDefaultFrozen
      ConfigAssetUnitName ConfigAssetName ConfigAssetURL
      ConfigAssetMetadataHash ConfigAssetManager ConfigAssetReserve
      ConfigAssetFreeze ConfigAssetClawback FreezeAsset
      FreezeAssetAccount FreezeAssetFrozen)
     2]
    [(Assets NumAssets Applications NumApplications GlobalNumUint
      GlobalNumByteSlice LocalNumUint LocalNumByteSlice)
     3]
    [(ExtraProgramPages)
     4]
    [(Nonparticipation Logs NumLogs CreatedAssetID CreatedApplicationID)
     5]))

#|
(provide (sumtype-out TransactionField1)
         (sumtype-out TransactionField2)
         (sumtype-out TransactionField3)
         (sumtype-out TransactionField4)
         (sumtype-out TransactionField5)
         (sumtype-out TransactionField6)
         transaction-field-name
         read-transaction-field
         transaction-field-logic-signature-version)

(provide (sumtype-out TransactionField1)
         (sumtype-out TransactionField2)
         (sumtype-out TransactionField3)
         (sumtype-out TransactionField4)
         (sumtype-out TransactionField5)
         (sumtype-out TransactionField6)
         transaction-field-name
         read-transaction-field
         transaction-field-logic-signature-version)

(define-sumtype GlobalField4
  GlobalField3)

(define-sumtype GlobalField5
  GlobalField4
  (CurrentApplicationAddress)
  (GroupID))

(define global-field
  (index→enumtype GlobalField))

(define global-field-name
  (sumtype-name GlobalField))

(define (read-global-field rb)
  (match-define (ReadByte [monad (Monad unit >>=)]) rb)
  (>>= (read-uint8 rb) (λ (i) (unit (global-field i)))))

(define global-field-logic-signature-version
  (enumtype-case-lambda GlobalField
    [(MinTxnFee MinBalance MaxTxnLife ZeroAddress GroupSize)
     1]
    [(LogicSigVersion Round LatestTimestamp CurrentApplicationID)
     2]
    [(CreatorAddress)
     3]
    [(CurrentApplicationAddress GroupID)
     5]))

(provide (sumtype-out GlobalField)
         global-field-name
         read-global-field
         global-field-logic-signature-version)


(define asset-holding-field
  (index→enumtype AssetHoldingField))

(define asset-holding-field-name
  (sumtype-name AssetHoldingField))

(define (read-asset-holding-field rb)
  (match-define (ReadByte [monad (Monad unit >>=)]) rb)
  (>>= (read-uint8 rb) (λ (i) (unit (asset-holding-field i)))))

(provide (sumtype-out AssetHoldingField)
         asset-holding-field-name
         read-asset-holding-field)

(define-sumtype AssetParamsfield5
  AssetParamsField2
  (AssetCreator))

(define asset-params-field
  (index→enumtype AssetParamsField))

(define asset-params-field-name
  (sumtype-name AssetParamsField))

(define (read-asset-params-field rb)
  (match-define (ReadByte [monad (Monad unit >>=)]) rb)
  (>>= (read-uint8 rb) (λ (i) (unit (asset-params-field i)))))

(define asset-params-field-logic-signature-version
  (enumtype-case-lambda AssetParamsField
    [(AssetTotal AssetDecimals AssetDefaultFrozen AssetUnitName AssetName AssetURL
      AssetMetadataHash AssetManager AssetReserve AssetFreeze AssetClawback)
     1]
    [(AssetCreator)
     5]))

(provide (sumtype-out AssetParamsField)
         asset-params-field-name
         read-asset-params-field
         asset-params-field-logic-signature-version)
|#

#;
(define app-params-field
  (index→enumtype AppParamsField))

#;
(define app-params-field-name
  (sumtype-name AppParamsField))

#;
(define (read-app-params-field rb)
  (match-define (ReadByte [monad (Monad unit >>=)]) rb)
  (>>= (read-uint8 rb) (λ (i) (unit (app-params-field i)))))

#;
(provide (sumtype-out AppParamsField)
         app-params-field-name
         read-app-params-field)






#|



(define instruction-name
  (sumtype-name Instruction))

(define-match-expander range
  (syntax-rules ()
    [(_ id lb ub)
     (and id (? (λ (oc) (and (r:<= lb oc) (r:<= oc ub)))))]))

(define (unused-opcode opcode)
  (error 'read-instruction "opcode ~a is unused" (number->string opcode 16)))

; ReadByte => m Instruction
(define (read-instruction rb)
  (match-define (ReadByte [monad (Monad unit >>=)]) rb)
  (define read-offset read-int16)
  (>>= (read-opcode rb) 
       (match-lambda
         [#x00 (unit (err))]
         [#x01 (unit (sha256))]
         [#x02 (unit (keccak256))]
         [#x03 (unit (sha512_256))]
         [#x04 (unit (ed25519verify))]
         [#x05 (>>= (read-uint8 rb) (λ (v) (unit (ecdsa_verify v))))]
         [#x06 (>>= (read-uint8 rb) (λ (v) (unit (ecdsa_pk_decompress v))))]
         [#x07 (>>= (read-uint8 rb) (λ (v) (unit (ecdsa_pk_recover v))))]
         [#x08 (unit (+))]
         [#x09 (unit (-))]
         [#x0a (unit (/))]
         [#x0b (unit (*))]
         [#x0c (unit (<))]
         [#x0d (unit (>))]
         [#x0e (unit (<=))]
         [#x0f (unit (>=))]
         [#x10 (unit (&&))]
         [#x11 (unit (\|\|))]
         [#x12 (unit (==))]
         [#x13 (unit (!=))]
         [#x14 (unit (!))]
         [#x15 (unit (len))]
         [#x16 (unit (itob))]
         [#x17 (unit (btoi))]
         [#x18 (unit (%))]
         [#x19 (unit (\|))]
         [#x1a (unit (&))]
         [#x1b (unit (^))]
         [#x1c (unit (~))]
         [#x1d (unit (mulw))]
         [#x1e (unit (addw))]
         [#x1f (unit (divmodw))]
         [#x20 (>>= (read-intcblock rb) (λ (ns) (unit (intcblock [uints ns]))))]
         [#x21 (>>= (read-uint8 rb) (λ (i) (unit (intc i))))]
         [#x22 (unit (intc_0))]
         [#x23 (unit (intc_1))]
         [#x24 (unit (intc_2))]
         [#x25 (unit (intc_3))]
         [#x26 (>>= (read-bytecblock rb) (λ (bss) (unit (bytecblock [bytess bss]))))]
         [#x27 (>>= (read-uint8 rb) (λ (i) (unit (bytec i))))]
         [#x28 (unit (bytec_0))]
         [#x29 (unit (bytec_1))]
         [#x2a (unit (bytec_2))]
         [#x2b (unit (bytec_3))]
         [#x2c (>>= (read-uint8 rb) (λ (n) (unit (arg n))))]
         [#x2d (unit (arg_0))]
         [#x2e (unit (arg_1))]
         [#x2f (unit (arg_2))]
         [#x30 (unit (arg_3))]
         [#x31 (>>= (read-transaction-field rb) (λ (f) (unit (txn [field f]))))]
         [#x32 (>>= (read-global-field rb) (λ (f) (unit (global [field f]))))]
         [#x33 (>>= (read-uint8 rb) (λ (gi) (>>= (read-transaction-field rb) (λ (f) (unit (gtxn [group-index gi] [field f]))))))]
         [#x34 (>>= (read-uint8 rb) (λ (i) (unit (load i))))]
         [#x35 (>>= (read-uint8 rb) (λ (i) (unit (store i))))]
         [#x36 (>>= (read-transaction-field rb) (λ (f) (>>= (read-uint8 rb) (λ (ai) (unit (txna [field f] [array-index ai]))))))]
         [#x37 (>>= (read-uint8 rb) (λ (gi) (>>= (read-transaction-field rb) (λ (f) (>>= (read-uint8 rb) (λ (ai) (unit (gtxna [group-index gi] [field f] [array-index ai]))))))))]
         [#x38 (>>= (read-transaction-field rb) (λ (f) (unit (gtxns [field f]))))]
         [#x39 (>>= (read-transaction-field rb) (λ (f) (>>= (read-uint8 rb) (λ (ai) (unit (gtxnsa [field f] [array-index ai]))))))]
         [#x3a (>>= (read-uint8 rb) (λ (gi) (>>= (read-uint8 rb) (λ (i) (unit (gload [group-index gi] i))))))]
         [#x3b (>>= (read-uint8 rb) (λ (i) (unit (gloads i))))]
         [#x3c (>>= (read-uint8 rb) (λ (gi) (unit (gaid [group-index gi]))))]
         [#x3d (unit (gaids))]
         [#x3e (unit (loads))]
         [#x3f (unit (stores))]
         [#x40 (>>= (read-offset rb) (λ (offset) (unit (bnz offset))))]
         [#x41 (>>= (read-offset rb) (λ (offset) (unit (bz offset))))]
         [#x42 (>>= (read-offset rb) (λ (offset) (unit (b offset))))]
         [#x43 (unit (return))]
         [#x44 (unit (assert))]
         [(range opcode #x45 #x47) (unused-opcode opcode)]
         [#x48 (unit (pop))]
         [#x49 (unit (dup))]
         [#x4a (unit (dup2))]
         [#x4b (>>= (read-uint8 rb) (λ (n) (unit (dig n))))]
         [#x4c (unit (swap))]
         [#x4d (unit (select))]
         [#x4e (>>= (read-uint8 rb) (λ (n) (unit (cover n))))]
         [#x4f (>>= (read-uint8 rb) (λ (n) (unit (uncover n))))]
         [#x50 (unit (concat))]
         [#x51 (>>= (read-uint8 rb) (λ (s) (>>= (read-uint8 rb) (λ (e) (unit (substring [start s] [end e]))))))]
         [#x52 (unit (substring3))]
         [#x53 (unit (getbit))]
         [#x54 (unit (setbit))]
         [#x55 (unit (getbyte))]
         [#x56 (unit (setbyte))]
         [#x57 (>>= (read-uint8 rb) (λ (s) (>>= (read-uint8 rb) (λ (l) (unit (extract [start s] [length l]))))))]
         [#x58 (unit (extract3))]
         [#x59 (unit (extract_uint16))]
         [#x5a (unit (extract_uint32))]
         [#x5b (unit (extract_uint64))]
         [(range opcode #x5c #x5f) (unused-opcode opcode)]
         [#x60 (unit (balance))]
         [#x61 (unit (app_opted_in))]
         [#x62 (unit (app_local_get))]
         [#x63 (unit (app_local_get_ex))]
         [#x64 (unit (app_global_get))]
         [#x65 (unit (app_global_get_ex))]
         [#x66 (unit (app_local_put))]
         [#x67 (unit (app_global_put))]
         [#x68 (unit (app_local_del))]
         [#x69 (unit (app_global_del))]
         [(range opcode #x6a #x6f) (unused-opcode opcode)]
         [#x70 (>>= (read-asset-holding-field rb) (λ (f) (unit (asset_holding_get [field f]))))]
         [#x71 (>>= (read-asset-params-field rb) (λ (f) (unit (asset_params_get [field f]))))]
         [#x72 (>>= (read-app-params-field rb) (λ (f) (unit (asset_params_get [field f]))))]
         [(range opcode #x73 #x77) (unused-opcode opcode)]
         [#x78 (unit (min_balance))]
         [(range opcode #x79 #x7f) (unused-opcode opcode)]
         [#x80 (>>= (read-bytes rb) (λ (bs) (unit (pushbytes [bytes bs]))))]
         [#x81 (>>= (read-varuint rb) (λ (n) (unit (pushint [uint n]))))]
         [(range opcode #x82 #x87) (unused-opcode opcode)]
         [#x88 (>>= (read-offset rb) (λ (offset) (unit (callsub offset))))]
         [#x89 (unit (retsub))]
         [(range opcode #x8a #x8f) (unused-opcode opcode)]
         [#x90 (unit (shl))]
         [#x91 (unit (shr))]
         [#x92 (unit (sqrt))]
         [#x93 (unit (bitlen))]
         [#x94 (unit (exp))]
         [#x95 (unit (expw))]
         [(range opcode #x96 #x9f) (unused-opcode opcode)]
         [#xa0 (unit (b+))]
         [#xa1 (unit (b-))]
         [#xa2 (unit (b/))]
         [#xa3 (unit (b*))]
         [#xa4 (unit (b<))]
         [#xa5 (unit (b>))]
         [#xa6 (unit (b<=))]
         [#xa7 (unit (b>=))]
         [#xa8 (unit (b==))]
         [#xa9 (unit (b!=))]
         [#xaa (unit (b%))]
         [#xab (unit (b\|))]
         [#xac (unit (b&))]
         [#xad (unit (b^))]
         [#xae (unit (b~))]
         [#xaf (unit (bzero))]
         [#xb0 (unit (log))]
         [#xb1 (unit (itxn_begin))]
         [#xb2 (>>= (read-transaction-field rb) (λ (f) (unit (itxn_field [field f]))))]
         [#xb3 (unit (itxn_submit))]
         [#xb4 (>>= (read-transaction-field rb) (λ (f) (unit (itxn [field f]))))]
         [#xb5 (>>= (read-transaction-field rb) (λ (f) (>>= (read-uint8 rb) (λ (ai) (unit (itxna [field f] [array-index ai]))))))]
         [#xb6 (unit (itxn_next))]
         [(range opcode #xb7 #xbf) (unused-opcode opcode)]
         [#xc0 (>>= (read-transaction-field rb) (λ (f) (unit (txnas [field f]))))]
         [#xc1 (>>= (read-uint8 rb) (λ (i) (>>= (read-transaction-field rb) (λ (f) (unit (gtxnas [group-index i] [field f]))))))]
         [#xc2 (>>= (read-transaction-field rb) (λ (f) (unit (gtxnsas [field f]))))]
         [#xc3 (unit (args))]
         [(range opcode #xc4 #xff) (unused-opcode opcode)])))

; instruction-logic-signature-version : Instruction -> positive-integer?
(define instruction-logic-signature-version
  (enumtype-case-lambda Instruction
    [(err sha256 keccak256 sha512_256 ed25519verify + - / * < > <= >= && \|\| == !=
      ! len itob btoi % \| & ^ ~ mulw intcblock intc intc_0 intc_1 intc_2 intc_3
      bytecblock bytec bytec_0 bytec_1 bytec_2 bytec_3 arg arg_0 arg_1 arg_2 arg_3
      txn global gtxn load store bnz pop dup)
     1]
    [(addw txna gtxna bz b return dup2 concat substring substring3 balance app_opted_in
      app_local_get app_local_get_ex app_global_get app_global_get_ex app_local_put
      app_global_put app_local_del app_global_del asset_holding_get asset_params_get)
     2]
    [(gtxns gtxnsa assert dig swap select getbit setbit getbyte setbyte min_balance
      pushbytes pushint)
     3]
    [(divmodw gload gloads gaid gaids callsub retsub shl shr sqrt bitlen exp expw b+
              b- b/ b* b< b> b<= b>= b== b!= b% b\| b& b^ b~ bzero)
     4]
    [(ecdsa_verify ecdsa_pk_decompress ecdsa_pk_recover loads stores cover uncover
      extract extract3 extract_uint16 extract_uint32 extract_uint64 app_params_get
      log itxn_begin itxn_field itxn_submit itxn itxna txnas gtxnas gtxnsas args)
     5]
    [(itxn_next)
     6]))

(provide (sumtype-out Instruction)
         instruction-name
         read-instruction
         instruction-logic-signature-version)

; instruction-cost : LogicSigVersion => Instruction -> m positive-integer?
(define (instruction-cost lsv)
  (match-define (LSV [monad (Monad unit >>=)] logic-sig-version) lsv)
  (define (cost/v1-shift v1-cost v2+-cost)
    (>>= logic-sig-version
         (match-lambda
           [1 (unit v1-cost)]
           [_ (unit v2+-cost)])))
  (sumtype-case-lambda Instruction
    [(sha256)
     (cost/v1-shift 7 35)]
    [(keccak256)
     (cost/v1-shift 26 130)]
    [(sha512_256)
     (cost/v1-shift 9 45)]
    [(ed25519verify)
     (unit 1900)]
    [(ecdsa_verify)
     (unit 1700)]
    [(ecdsa_pk_decompress)
     (unit 650)]
    [(ecdsa_pk_recover)
     (unit 2000)]
    [(divmodw)
     (unit 20)]
    [(sqrt)
     (unit 4)]
    [(b+)
     (unit 10)]
    [(b-)
     (unit 10)]
    [(b/)
     (unit 10)]
    [(b*)
     (unit 10)]
    [(b%)
     (unit 20)]
    [(b\|)
     (unit 6)]
    [(b&)
     (unit 6)]
    [(b^)
     (unit 6)]
    [(b~)
     (unit 4)]
    #:otherwise _
    (unit 1)))

(provide instruction-cost)
|#
