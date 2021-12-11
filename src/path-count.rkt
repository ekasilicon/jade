#lang racket/base
(require racket/match
         racket/set
         (only-in racket/list append-map)
         "static/record.rkt"
         "static/sumtype.rkt"
         (prefix-in i: "instruction.rkt")
         "monad.rkt"
         "read-byte.rkt"
         "logic-sig-version.rkt"
         "prefix.rkt")

(require racket/pretty)

(define-sumtype Result
  (success values state)
  (failure message))

(define ((unit . values) state) (success values state))
(define ((>>= m f) st)
  (sumtype-case Result (m st)
    [(success [values xs] [state st])
     ((apply f xs) st)]
     #:otherwise x
     (pretty-print x)
     (raise 'stop)))

(define pc-Monad (Monad unit >>=))

(define pc-ReadByte
  (ReadByte [monad pc-Monad]
            [read-byte (λ (st)
                         (let ([bytecode (hash-ref st 'bytecode)]
                               [pc       (hash-ref st 'pc)])
                           (if (< pc (bytes-length bytecode))
                             (success [values (list (bytes-ref bytecode pc))]
                                      [state (hash-update st 'pc add1)])
                             (failure [message "read out of bounds"]))))]))

(define pc-LogicSigVersion
  (LogicSigVersion [monad pc-Monad]
                   [logic-sig-version (λ (st) (success [values (list (hash-ref st 'lsv))] [state st]))]))

(define (jump st offset)
  (hash-update st 'pc (λ (pc) (+ pc offset))))

; the number of SMT solver queries that need to be made
(define (query-count st)
  (let ([bytecode       (hash-ref st 'bytecode)]
        [pc             (hash-ref st 'pc)]
        [execution-cost (hash-ref st 'execution-cost)])
    (if (or (= pc (bytes-length bytecode))
            (> execution-cost 700))
      0
      (sumtype-case Result ((i:read-instruction pc-ReadByte) st) 
        [(success [values (list instr)]
                  [state st])
         (let ([st (sumtype-case Result (((i:instruction-cost pc-LogicSigVersion) instr) st) 
                     [(success [values (list ic)] [state st])
                      (hash-update st 'execution-cost (λ (ec) (+ ec ic)))]
                     #:otherwise _
                     st)])
           (sumtype-case i:Instruction instr
           [(i:b offset)
            (query-count (jump st offset))]
           [(i:bz offset)
            (+ 1 (query-count st)
               1 (query-count (jump st offset)))]
           [(i:bnz offset)
            (+ 1 (query-count st)
               1 (query-count (jump st offset)))]
           #:otherwise _
           (query-count st)))]
        #:otherwise _
        0))))

(define (inject bytecode)
  (match ((read-varuint prefix-ReadByte) bytecode)
    [(cons lsv bytecode)
     (hasheq 'LogiSigVersion lsv
             'bytecode bytecode
             'pc       0
             ;'pathseen (seteqv)
             'execution-cost 0
             )]))

#;
(define (step vm)
  (match-let* ([(VM monad+ [read-byte rb]) vm]
               [(Monad+ monad [mplus each]) monad+]
               [(Monad unit >>=) monad]
               [>> (>> monad)]
               [fail (mzero monad+)])
    (define ((jump offset) st)
      (let ([bytecode (hash-ref st 'bytecode)]
            [pc       (hash-ref st 'pc)])
        (list (success [values (list)]
                       [state (hash-set st 'pc (+ pc offset))]))))
    (define (check-final st)
      (let ([bytecode (hash-ref st 'bytecode)]
            [pc       (hash-ref st 'pc)])
        (if (= pc (bytes-length bytecode))
          (list (finishe))
          (list (success [values (list)]
                         [state st])))
        (list (success [values (list)]
                       [state (hash-set st 'pc (+ pc offset))]))))
    (>> (>>= (i:read-instruction rb)
             (sumtype-case-lambda i:Instruction
               [(i:b offset)
                (jump offset)]
               [(i:bz offset)
                (>> branch++
                    (each (jump offset) (unit)))]
               [(i:bnz offset)
                (>> branch++
                    (each (jump offset) (unit)))]
               [(i:callsub) (raise 'callsub)]
               #:otherwise instr
               (unit)))
        check-final)))

(module+ main
  (require "../test/algoexplorer/extract.rkt")

  (for-each
   (λ (path)
     (let ([st (inject (file-extract path 'approval-program))])
       (displayln path)
       (displayln (time (query-count st)))))
   (vector->list (current-command-line-arguments)))
  
  #;
  (let loop ([count 0]
             [st (time (inject (port->bytes (current-input-port))))])
    (foldl
     (λ (r cnt)
       (sumtype-case Result r
         [(success [values (list)]
                   [state st])
          (loop cnt st)]
         [(failure)
          cnt]))
     count
     ((step pc-VM) st))))
