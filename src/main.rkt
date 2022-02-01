#lang racket/base
(require racket/match
         (rename-in racket/cmdline [command-line r:command-line])
         racket/port
         json
         net/base64
         "error.rkt"
         "parse.rkt"
         "disassemble.rkt"
         "assembly/show.rkt"
         "unconstrained-property-analysis.rkt")

(define (command-line)
  (with-handlers ([exn:fail? (λ (e) (error 'racket-error (exn-message e)))])
    (let ([mapped-constants (hash)]
          [mode #f])
      (r:command-line
       #:program "jade"
       #:argv (current-command-line-arguments)
       #:usage-help
       ""
       "jade analyzes the TEAL program bytecode supplied to standard input."
       #:multi
       [("--symbolic-bytes" "-b") symbol bytes-constant
        ("Treats the use of <bytes-constant> in a TEAL program"
         "as the symbolic value <symbol>."
         "The acceptable syntaxes of <bytes-constant> are those"
         "acceptable to the official TEAL assembler."
         "This option is intended for TEAL program templates"
         "in which certain bytes constants can be instantiated"
         "to create a program."
         "By instantiating these constants with unique dummy"
         "values, jade can analyze an assembled program"
         "which is representative of all programs which"
         "complete the template.")
        (let ([symbol (string->symbol symbol)]
              [parsed-bytes-constant (parse-bytes bytes-constant)])
          (cond
            [(hash-ref mapped-constants parsed-bytes-constant #f)
             => (match-lambda
                  [(cons symbol₀ bytes-constant₀)
                   (unless (eq? symbol₀ symbol)
                     (printf "Cannot associate equivalent bytes literals ~a and ~a with distinct symbols <~a> and <~a>.\n"
                             bytes-constant₀
                             bytes-constant
                             symbol₀
                             symbol)
                     (exit 255))])]
            [else
             (set! mapped-constants (hash-set mapped-constants parsed-bytes-constant (cons symbol bytes-constant)))]))]
       [("--symbolic-uint" "-u") symbol uint-constant
        ("Treats the use of <uint-constant> in a TEAL program"
         "as the symbolic value <symbol>."
         "This option is intended for TEAL program templates"
         "in which certain uint constants can be instantiated"
         "to create a program."
         "By instantiating these constants with unique dummy"
         "values, jade can analyze an assembled program"
         "which is representative of all programs which"
         "complete the template.")
        (let ([symbol (string->symbol symbol)]
              [parsed-uint-constant (parse-varuint uint-constant)])
          (cond
            [(hash-ref mapped-constants parsed-uint-constant #f)
             => (match-lambda
                  [(cons symbol₀ uint-constant₀)
                   (unless (eq? symbol₀ symbol)
                     (printf "Cannot associate equivalent uint literals ~a and ~a with distinct symbols <~a> and <~a>.\n"
                             uint-constant₀
                             uint-constant
                             symbol₀
                             symbol)
                     (exit 255))])]
            [else
             (set! mapped-constants (hash-set mapped-constants parsed-uint-constant (cons symbol uint-constant)))]))]
       #:once-any
       [("--json-package")
        ("Tell jade to expect a JSON-encoded bytecode package which"
         "includes both Approval and ClearState programs and useful"
         "metadata."
         "jade uses this additional information to perform a better"
         "analysis and produce better diagnostics."
         "The Application endpoint of the Algorand API v2 produces"
         "acceptable packages."
         "This API is implemented by the AlgoExplorer at"
         ""
         "  https://algoexplorer.io"
         ""
         "with documentation at"
         ""
         "  https://algoexplorer.io/api-dev/v2"
         "")
        (set! mode 'json-package)]
       [("--raw-binary")
        ("Tell jade to expect raw bytecode binary on standard input")
        (set! mode 'raw-binary)]
       [("--assembly")
        ("Tell jade to expect assembly on standard input")
        (set! mode 'assembly)]
       #:usage-help
       "It expects either the --json-package flag or the --raw-binary flag,"
       "each documented below."
       #:args ()
       (define (standard-input-bytes expected)
         (let ([bs (port->bytes (current-input-port))])
           (if (bytes=? bs #"")
             (error 'no-standard-input "Expected ~a on standard input." expected)
             bs)))
       (match mode
         ['raw-binary
          (error->>= (standard-input-bytes "raw binary")
                     (λ (bs) (λ () (error->>= (disassemble bs)
                                              (λ (asm)
                                                (list (cons 'assembly (assembly-show asm))
                                                      (cons 'UPA      (UPA asm #f mapped-constants))))))))]
         ['assembly
          (error->>= (standard-input-bytes "assembly")
                     (λ (bs) (λ () (error->>= (parse (bytes->string/utf-8 bs))
                                              (λ (asm)
                                                (list (cons 'assembly (assembly-show asm))
                                                      (cons 'UPA      (UPA asm #f mapped-constants))))))))]
         ['json-package
          (error->>= (standard-input-bytes "JSON package")
                     (λ (bs)
                       (λ ()
                         (error->>= (with-handlers ([exn:fail:read? (λ (e) (error 'invalid-json "Package input not valid JSON"))])
                                      (read-json (open-input-bytes bs)))
                                    (match-lambda
                                      [(hash-table ('id id)
                                                   ('params (and params
                                                                 (hash-table ('approval-program    approval-program)
                                                                             ('clear-state-program clear-state-program)
                                                                             ('creator             creator)
                                                                             ('global-state-schema (hash-table ('num-byte-slice global-num-byte-slice)
                                                                                                               ('num-uint       global-num-uint)))
                                                                             ('local-state-schema  (hash-table ('num-byte-slice local-num-byte-slice)
                                                                                                               ('num-uint       local-num-uint)))))))
                                       (if (or (eq? approval-program 'null)
                                               (eq? clear-state-program 'null))
                                         (error 'program-missing "Program was null in package")
                                         (let ([global-state (match params
                                                               [(hash-table ('global-state global-entries))
                                                                (for/hash ([entry (in-list global-entries)])
                                                                  (match entry
                                                                    [(hash-table ('key key) ('value value))
                                                                     (values (base64-decode (string->bytes/utf-8 key))
                                                                             (match (hash-ref value 'type)
                                                                               [1 (base64-decode (string->bytes/utf-8 (hash-ref value 'bytes)))]
                                                                               [2 (hash-ref value 'uint)]))]))]
                                                               [_
                                                                #f])])
                                           (error->>= (disassemble (base64-decode (string->bytes/utf-8 approval-program)))
                                                      (λ (asm)
                                                        (list (cons 'assembly (assembly-show asm))
                                                              (cons 'UPA      (UPA asm
                                                                                   (execution-context [approval-program     (base64-decode (string->bytes/utf-8 approval-program))]
                                                                                                      [clear-state-program  (base64-decode (string->bytes/utf-8 clear-state-program))]
                                                                                                      global-num-byte-slice
                                                                                                      global-num-uint
                                                                                                      local-num-byte-slice
                                                                                                      local-num-uint
                                                                                                      global-state)
                                                                                   mapped-constants)))))))]
                                      [json
                                       (error 'invalid-package-format "Input JSON did not match expected format. (Was it produced by the Algorand API v2?)")])))))]
         [#f
          (match (current-command-line-arguments)
            ; no arguments at all
            [(vector)
             (error 'no-arguments
                    #<<MESSAGE
usage: jade [ <option> ... ]
  
jade analyzes the TEAL program bytecode supplied to standard input.
It expects either the --json-package flag or the --raw-binary flag.
Guidance for use of these flags, and others, is avaliable via the
command

                        jade --help

MESSAGE
                )]
            [_
             (error 'bad-flag "Expected either --raw-binary or --json-package flag.")])])))))

(provide command-line)

(module+ main
  (define result
    (command-line))

  (uncaught-exception-handler
   (λ (e)
     (displayln "jade internal error:")
     (cond
       [(exn? e)
        (displayln (exn-message e))]
       [else
        (displayln e)])
     (exit 255)))

  (define (report-error tag message)
    (display "ERROR ")
    (displayln message)
    (exit 255))
  
  (match result
    [(error-result tag message)
     (report-error tag message)]
    [action
     (match (action)
       [(error-result tag message)
        (report-error tag message)]
       [reports
        (for-each
         (λ (report)
           (displayln report)
           (newline))
         (map cdr reports))])]))




