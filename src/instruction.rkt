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
        (match-lambda
          [0  (unit (AssetTotal))]
          [1  (unit (AssetDecimals))]
          [2  (unit (AssetDefaultFrozen))]
          [3  (unit (AssetUnitName))]
          [4  (unit (AssetName))]
          [5  (unit (AssetURL))]
          [6  (unit (AssetMetadataHash))]
          [7  (unit (AssetManager))]
          [8  (unit (AssetReserve))]
          [9  (unit (AssetFreeze))]
          [10 (unit (AssetClawback))]
          [11 (unit (AssetCreator))])]))

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
   42))
