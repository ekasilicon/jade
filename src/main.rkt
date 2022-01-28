#lang racket/base
(require racket/match
         racket/cmdline
         racket/port
         "error.rkt"
         "parse.rkt"
         "assembly/show.rkt"
         "unconstrained-property-analysis.rkt")

(uncaught-exception-handler
 (λ (e)
   (displayln "jade internal error:")
   (cond
     [(exn? e)
      (displayln (exn-message e))]
     [else
      (displayln e)])
   (exit 255)))

(define result
  (let ([constants (hash)]
        [mode #f])
    (command-line
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
          [(hash-ref constants parsed-bytes-constant #f)
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
           (set! constants (hash-set constants parsed-bytes-constant (cons symbol bytes-constant)))]))]
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
          [(hash-ref constants parsed-uint-constant #f)
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
           (set! constants (hash-set constants parsed-uint-constant (cons symbol uint-constant)))]))]
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
      (set! mode (cons 'json-package #f))]
     [("--raw-binary") program-type
      ("Tell jade to expect raw bytecode binary on standard input"
       "with <program-type> ApprovalProgram or ClearStateProgram.")
      (match program-type
        ["ApprovalProgram"
         (set! mode (cons 'raw-binary 'ApprovalProgram))]
        ["ClearStateProgram"
         (set! mode (cons 'raw-binary 'ClearStateProgram))]
        [_
         (error 'bad-program-type #<<MESSAGE
Received ~a as <program-type> but expected either ApprovalProgram
or ClearStateProgram.
MESSAGE
               program-type)])]
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
       [(cons 'raw-binary program-type)
        (error->>= (standard-input-bytes "raw binary")
                   (λ (bs) (analyze/raw-binary program-type bs constants)))]
       [(cons 'json-package #f)
        (error->>= (standard-input-bytes "JSON package")
                   (λ (bs) (analyze/json-package bs constants)))]
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
           (error 'bad-flag "Expected either --raw-binary or --json-package flag.")])]))))

(match result
  [(error-result tag message)
   (display "ERROR ")
   (displayln message)
   (exit 255)]
  [report
   (displayln report)])
