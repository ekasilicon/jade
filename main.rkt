#lang racket/base
(require racket/match
         racket/cmdline
         racket/port
         #;
         "src/incremental.rkt")

(let ([constants (hash)])
  (command-line
   #:program "jade"
   #:argv (match (current-command-line-arguments)
            [(vector)
             (vector "--help")]
            [ccla ccla])
   #:usage-help
   ""
   "\010\010jade analyzes the TEAL program bytecode supplied to standard input."
   ""
   "\010\010<bytecode-format> is one of"
   ""
   "raw-binary"
   "   Tell jade to expect raw bytecode binary on standard input."
   "garnished-json"
   "   Tell jade to expect a JSON-encoded bytecode package which"
   "   includes both Approval and ClearState programs and useful"
   "   metadata."
   "   jade uses this additional information to perform a better"
   "   analysis and produce better diagnostics."
   "   The Application endpoint of the Algorand API v2 produces"
   "   acceptable packages."
   "   This API is implemented by the AlgoExplorer at"
   ""
   "     https://algoexplorer.io"
   ""
   "   with documentation at"
   ""
   "      https://algoexplorer.io/api-dev/v2"
   #:multi
   [("--symbolic-bytes" "-b") symbol bytes-constant
    ("Treats the use of <bytes-constant> in a TEAL program"
     "as the symbolic value <symbol>."
     "The acceptable syntaxes of <bytes-constant> are those"
     "acceptable to the official TEAL assembler."
     "This option is intended for TEAL program templates"
     "in which certain byte sconstants can be instantiated"
     "to create a program."
     "By instantiating these constants with unique dummy"
     "values, jade can analyze an assembled program"
     "which is representative of all programs which"
     "complete the template.")
    (let ([symbol (string->symbol symbol)]
          [parsed-bytes-constant (string->bytes/utf-8 bytes-constant)])
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
          [parsed-uint-constant (string->number uint-constant)])
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
   #:args (bytecode-format)
   (match bytecode-format
     ["raw-binary"
      (let ([bs (port->bytes (current-input-port))])
        (if (bytes=? bs #"")
          (begin
            (displayln "Expected raw binary on standard input.")
            (exit 255))
          (void)
          #;
          (analyze/raw-binary bs (make-immutable-hasheq (hash->list constants)))))]
     ["garnished-json"
      (let ([bs (port->bytes (current-input-port))])
        (if (bytes=? bs #"")
          (begin
            (displayln "Expected JSON package on standard input.")
            (exit 255))
          (void)
          #;
          (analyze/json-package bs (make-immutable-hasheq (hash->list constants)))))]
     [_
      (printf #<<MESSAGE
Expected <bytecode-format> of 'raw-binary' or 'garnished-json'
but got '~a'. The command

  jade --help

offers more information about the acceptable options.

MESSAGE
              bytecode-format)])))

; put this handler after the command-line parsing because
; `command-line` exceptions are caught by it otherwise.
(uncaught-exception-handler
 (λ (e)
   (displayln "jade internal error:")
   (cond
     [(exn? e)
      (displayln (exn-message e))]
     [else
      (displayln e)])
   (exit 255)))
