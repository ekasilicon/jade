#lang racket/base
(require racket/match
         (rename-in racket/cmdline [command-line r:command-line])
         racket/port
         json
         net/base64
         "jade-error.rkt"
         "parse.rkt"
         "disassemble.rkt"
         "assembly/show.rkt"
         "execution-context.rkt"
         "analyses/unconstrained-property-analysis.rkt"
         "analyses/typecheck.rkt")

(define (command-line)
  (with-handlers ([jade-error? (λ (e) e)]
                  [exn:fail? (λ (e) (jade-error 'racket-error (exn-message e)))])
    (let ([mapped-constants (hash)]
          [assembly? #t]
          [UPA? #f]
          [typecheck? #t]
          [mode #f])
      (r:command-line
       #:program "jade"
       #:argv (current-command-line-arguments)
       #:usage-help
       ""
       "jade typechecks the TEAL program bytecode supplied to standard input"
       "and optionally runs the Unconstrained Property Analysis."
       ""
       "The typechecker determines:"
       ""
       "  1. that each operation in the program is applied to type-correct arguments, and"
       "  2. that a pop operation is never performed on an empty stack."
       ""
       "It does not distinguish between the scratchspaces of different preceding"
       "contracts in the transaction group."
       ""
       "It works only with statically-known keys for global and local storage. Additionally,"
       "it requires that all values at the same key, regardless of the application ID, have"
       "the same type."
       "If the program is pulled from a JSON package, it checks that the globals at particular"
       "keys have the assumed type; if not, it prints the assumptions that underly its declar-"
       "aration of type safety."
       ""
       "The type system that it implements is simple in the technical sense. Therefore,"
       "it is possible for a program to actually be type correct but not typecheck."
       "One virtue of a simple type system such as this is that it accepts only"
       "type-correct code which is *obviously* type-correct."
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
         "complete the template."
         "Applies to the Unconstrained Property Analysis only.")
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
         "complete the template."
         "Applies to the Unconstrained Property Analysis only.")
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
       #:once-each
       [("--no-assembly-report")
        ("Tell jade not to print the assembly of the ingested"
         "program.")
        (set! assembly? #f)]
       [("--no-typecheck-report")
        ("Tell jade not to run the typechecker.")
        (set! typecheck? #f)]
       [("--UPA-report")
        ("Tell jade to run the Unconstrained Property Analysis (UPA)."
         "The UPA runs only for TEALv3 programs and earlier.")
        (set! UPA? #f)]
       #:once-any
       [("--json-package")
        ("Tell jade to expect a JSON-encoded bytecode package which"
         "includes both Approval and ClearState programs and useful"
         "metadata."
         "jade uses this additional information to perform a better"
         "analysis and produce better diagnostics."
         "The Application endpoint of the Algorand Indexer v2 produces"
         "acceptable packages."
         "Such an endpoint is implemented by the AlgoExplorer at"
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
       ""
       "It expects the --json-package, --raw-binary, or --assembly flag,"
       "each documented below."
       #:args ()
       (define (standard-input-bytes expected)
         (let ([bs (port->bytes (current-input-port))])
           (if (bytes=? bs #"")
             (error 'no-standard-input "Expected ~a on standard input." expected)
             bs)))
       (define (assembly-report description asm)
         (and assembly?
              (cons 'assembly (string-append description "\n" (assembly-show asm)))))
       (define (UPA-report asm ctx)
         (and UPA?
              (cons 'UPA (UPA asm ctx mapped-constants))))
       (define (typecheck-report asm ctx)
         (and typecheck?
              (cons 'typecheck (typecheck asm ctx))))
       (match mode
         ['raw-binary
          (λ ()
            (let ([asm (disassemble (standard-input-bytes "raw binary"))])
              (list (assembly-report "Disassembled instructions" asm)
                    (UPA-report asm #f)
                    (typecheck-report asm #f))))]
         ['assembly
          (λ ()
            (let ([asm (parse (bytes->string/utf-8 (standard-input-bytes "assembly")))])
              (list (assembly-report "Parsed instructions" asm)
                    (UPA-report asm #f)
                    (typecheck-report asm #f))))]
         ['json-package
          (λ ()
            (let loop ([json (with-handlers ([exn:fail:read? (λ (e) (jade-error 'invalid-json "Package input not valid JSON"))])
                               (read-json (open-input-bytes (standard-input-bytes "JSON package"))))])
              (match json
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
                   (jade-error 'program-missing "Program was null in package")
                   (let* ([global-state (match params
                                          [(hash-table ('global-state global-entries))
                                           (for/hash ([entry (in-list global-entries)])
                                             (match entry
                                               [(hash-table ('key key) ('value value))
                                                (values (base64-decode (string->bytes/utf-8 key))
                                                        (match (hash-ref value 'type)
                                                          [1 (base64-decode (string->bytes/utf-8 (hash-ref value 'bytes)))]
                                                          [2 (hash-ref value 'uint)]))]))]
                                          [_
                                           #f])]
                          [asm (disassemble (base64-decode (string->bytes/utf-8 approval-program)))])
                     (let ([ctx (execution-context [approval-program     (base64-decode (string->bytes/utf-8 approval-program))]
                                                   [clear-state-program  (base64-decode (string->bytes/utf-8 clear-state-program))]
                                                   global-num-byte-slice
                                                   global-num-uint
                                                   local-num-byte-slice
                                                   local-num-uint
                                                   global-state)])
                       (list (assembly-report "\nDisassembled instructions" asm)
                             (UPA-report asm ctx)
                             (typecheck-report asm ctx)))))]
                [(hash-table ('application application))
                 (loop application)]
                [(hash-table ('message message))
                 (jade-error 'algoexplorer message)]
                [json
                 (jade-error 'invalid-package-format "Input JSON did not match expected format. (Was it produced by Algorand Indexer v2?)")])))]
         [#f
          (match (current-command-line-arguments)
            ; no arguments at all
            [(vector)
             (jade-error 'no-arguments
                    #<<MESSAGE
usage: jade [ <option> ... ]
  
jade analyzes the TEAL program bytecode supplied to standard input.
It expects either the --json-package flag, the --raw-binary flag, or
the --assembly flag. Guidance for use of these flags, and others,
is avaliable via the command

                        jade --help

MESSAGE
                    )]
            [_
             (jade-error 'bad-flag "Expected the --json-package, --raw-binary, or --assembly flag.")])])))))

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

  (define (report-error tag message show-error?)
    (when show-error?
      (display "ERROR "))
    (displayln message)
    (exit 255))
  
  (match result
    [(jade-error tag message)
     (report-error tag message #f)]
    [action
     (match (action)
       [(jade-error tag message)
        (report-error tag message #t)]
       [reports
        (for-each
         (match-lambda
           [#f
            (void)]
           [(cons _ report)
            (match report
              [(jade-error tag message)
               (display "ERROR ")
               (display message)]
              [report
               (displayln report)])
            (newline)])
         reports)])]))




