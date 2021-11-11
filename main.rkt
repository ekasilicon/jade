#lang racket/base
(require racket/match
         racket/cmdline)

(let ([constants (make-hasheq)])
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
        [(hash-ref constants symbol #f)
         => (match-lambda
              [`(bytes ,parsed-bytes-constant₀ ,bytes-constant₀)
               (unless (bytes=? parsed-bytes-constant₀ parsed-bytes-constant)
                 (printf "Cannot associate symbol <~a> with bytes constants ~a and ~a.\n"
                         symbol
                         bytes-constant₀
                         bytes-constant)
                 (exit 255))]
              [`(uint ,_ ,uint-constant)
               (printf "Cannot associate symbol <~a> with uint constant ~a and bytes constant ~a.\n"
                       symbol
                       uint-constant
                       bytes-constant)
               (exit 255)])]
        [else
         (hash-set! constants symbol `(bytes ,parsed-bytes-constant ,bytes-constant))]))]
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
        [(hash-ref constants symbol #f)
         => (match-lambda
              [`(uint ,parsed-uint-constant₀ ,uint-constant₀)
               (unless (= parsed-uint-constant₀ parsed-uint-constant)
                 (printf "Cannot associate symbol <~a> with uint constants ~a and ~a.\n"
                         symbol
                         uint-constant₀
                         uint-constant)
                 (exit 255))]
              [`(bytes ,_ ,bytes-constant)
               (printf "Cannot associate symbol <~a> with bytes constant ~a and uint constant ~a.\n"
                       symbol
                       bytes-constant
                       uint-constant)
               (exit 255)])]
        [else
         (hash-set! constants symbol `(uint ,parsed-uint-constant ,uint-constant))]))]
   #:args (bytecode-format)
   (match bytecode-format
     ["raw-binary"
      (void)]
     ["garnished-json"
      (void)]
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
