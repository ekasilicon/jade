#lang racket/base
(require (only-in racket/match match-lambda)
         "../static/sumtype.rkt"
         "../static/object.rkt"
         "../version.rkt"
         "../instruction.rkt"
         "../read-byte.rkt")

(define instruction-read/version
  (make-*/version
   'instruction-read/version
   (inc (logic-sig-version
         read-uint8 read-opcode
         >>=)
        [read-instruction
         (>>= read-opcode decode-instruction)]
        [decode-instruction
         (λ (oc)
           (>>= logic-sig-version
                (λ (lsv)
                  (error 'read-instruction
                         "illegal opcode ~a for TEAL version ~a"
                        (number->string oc 16)
                        lsv))))]
        [read-transaction-field
         (>>= read-uint8 decode-transaction-field)]
        [decode-transaction-field
         (λ (tf)
           (>>= logic-sig-version
                (λ (lsv)
                  (error 'read-transaction-field
                         "illegal transaction field ~a for TEAL version ~a"
                        (number->string tf 16)
                        lsv))))]
        [read-global-field
         (>>= read-uint8 decode-global-field)]
        [decode-global-field
         (λ (gf)
           (>>= logic-sig-version
                (λ (lsv)
                  (error 'read-global-field
                         "illegal global field ~a for TEAL version ~a"
                        (number->string gf 16)
                        lsv))))]
        [read-asset-holding-field
         (>>= read-uint8 decode-asset-holding-field)]
        [decode-asset-holding-field
         (λ (ahf)
           (>>= logic-sig-version
                (λ (lsv)
                  (error 'read-asset-holding-field
                         "illegal asset holding field ~a for TEAL version ~a"
                        (number->string ahf 16)
                        lsv))))]
        [read-asset-params-field
         (>>= read-uint8 decode-asset-params-field)]
        [decode-asset-params-field
         (λ (apf)
           (>>= logic-sig-version
                (λ (lsv)
                  (error 'read-asset-params-field
                         "illegal asset params field ~a for TEAL version ~a"
                        (number->string apf 16)
                        lsv))))]
        [read-app-params-field
         (>>= read-uint8 decode-app-params-field)]
        [decode-app-params-field
         (λ (apf)
           (>>= logic-sig-version
                (λ (lsv)
                  (error 'read-app-params-field
                         "illegal app params field ~a for TEAL version ~a"
                        (number->string apf 16)
                        lsv))))])
   (inc (read-transaction-field read-global-field
         read-opcode read-uint8 read-offset read-intcblock read-bytecblock
         unit >>=)
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
           [oc   ((super 'decode-instruction) oc)])]
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
           [bc ((super 'decode-transaction-field) bc)])]
        [decode-global-field
         (match-lambda
           [0  (unit (MinTxnFee))]
           [1  (unit (MinBalance))]
           [2  (unit (MaxTxnLife))]
           [3  (unit (ZeroAddress))]
           [4  (unit (GroupSize))]
           [bc ((super 'decode-global-field) bc)])])
   (inc (read-transaction-field read-global-field read-asset-holding-field read-asset-params-field
        read-uint8 read-offset read-bytes read-varuint
        unit >>=)
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
           [oc   ((super 'decode-instruction) oc)])]
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
           [bc ((super 'decode-transaction-field) bc)])]
        [decode-global-field
         (match-lambda
           [5  (unit (LogicSigVersion))]
           [6  (unit (Round))]
           [7  (unit (LatestTimestamp))]
           [8  (unit (CurrentApplicationID))]
           [bc ((super 'decode-global-field) bc)])]
        [decode-asset-holding-field
         (match-lambda
           [0 (unit (AssetBalance))]
           [1 (unit (AssetFrozen))])]
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
           [11 (unit (AssetCreator))])])
   (inc (read-transaction-field read-global-field
         read-uint8 read-offset read-bytes read-varuint
         unit >>=)
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
           [oc   ((super 'decode-instruction) oc)])]
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
           [bc ((super 'decode-transaction-field) bc)])]
        [decode-global-field
         (match-lambda
           [9  (unit (CreatorAddress))]
           [bc ((super 'decode-global-field) bc)])])
   (inc (read-uint8 read-offset
         unit >>=)
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
           [oc   ((super 'decode-instruction) oc)])]
        [decode-transaction-field
         (match-lambda
           [56 (unit (ExtraProgramPages))]
           [bc ((super 'decode-transaction-field) bc)])])
   (inc (read-transaction-field read-app-params-field
         read-uint8
         unit >>=)
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
           [oc   ((super 'decode-instruction) oc)])]
        [decode-transaction-field
         (match-lambda
           [57 (unit (Nonparticipation))]
           [58 (unit (Logs))]
           [59 (unit (NumLogs))]
           [60 (unit (CreatedAssetID))]
           [61 (unit (CreatedApplicationID))]
           [bc ((super 'decode-transaction-field) bc)])]
        [decode-global-field
         (match-lambda
           [10 (unit (CurrentApplicationAddress))]
           [11 (unit (GroupID))]
           [bc ((super 'decode-global-field) bc)])]
        [decode-app-params-field
         (match-lambda
           [0 (unit (AppApprovalProgram))]
           [1 (unit (AppClearStateProgram))]
           [2 (unit (AppGlobalNumUint))]
           [3 (unit (AppGlobalNumByteSlice))]
           [4 (unit (AppLocalNumUint))]
           [5 (unit (AppLocalNumByteSlice))]
           [6 (unit (AppExtraProgramPages))]
           [7 (unit (AppCreator))]
           [8 (unit (AppAddress))])])
   (inc (unit)
        [decode-instruction
         (match-lambda
           [#xb6 (unit (itxn_next))]
           [oc   ((super 'decode-instruction) oc)])])
   read-byte-extras))

(provide instruction-read/version)

(module+ main
  (require (only-in racket/match match-let)
           "../read-byte.rkt")
  
  (((fix (mix (instruction-read/version 5)
              (inc ()
                   [unit (λ xs (λ (σ) (cons xs σ)))]
                   [>>= (λ (m f)
                          (λ (σ)
                            (match-let ([(cons xs σ) (m σ)])
                              ((apply f xs) σ))))])
              (inc (unit)
                   [logic-sig-version (unit 5)])
              read-byte-extras
              (inc (unit)
                   [read-byte (unit 3)])))
    'read-instruction)
   42))
