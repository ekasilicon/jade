#lang racket/base
(require racket/match
         racket/cmdline
         "help.rkt"
         "src/disassemble.rkt"
         "src/assembly.rkt")

(define (analyze arguments)
  (command-line #:program "jade analyze"
                #:argv arguments))

(define (disassemble arguments)
  (define file-path
    (command-line #:program "jade disassemble"
                  #:argv arguments
                  #:args (file-path) file-path))
  (if (file-exists? file-path)
    (emit! (call-with-input-file file-path disassemble-port))
    (begin
      (printf "disassemble: file does not exist at path ~s\n" file-path)
      (exit 255))))

#;
(command analyze some-argument-spec-and-help)

(match (current-command-line-arguments)
  [(or (vector)
       (vector "--help")
       (vector "-h"))
   (displayln #<<HELP
Usage: jade <command> <option> ... <arg> ...

Available commands:
  analyze               analyze a TEAL program
  disassemble           disassemble TEAL bytecode

See `jade <command> --help` for help on a command.
HELP
              )]
  [(vector command arguments ...)
   (match command
     ["analyze"
      (analyze arguments)]
     ["disassemble"
      (disassemble arguments)]
     [command
      (printf "jade: unknown command: ~a\n" command)
      (exit 255)])])

#;
(require racket/cmdline)


#;
(command-line #:program "jade"
              #:once-any
              [("--stdin" "-s")
               "Read TEAL program from standard input."
               (displayln "standard input")]
              [("--file" "-f") teal-program metadata
               "Read TEAL program from file."
               (displayln "from file")]
              [("--app-id" "-a") app-id
               "Fetch TEAL program from block explorer."
               (printf "fetch ~s\n" app-id)]
              #:once-any
              [("--bytecode" "-b")
               "Expect TEAL program assembled as bytecode. (default)"
               (printf "bytecoded\n")]
              [("--assembly" "-z")
               "Expect TEAL program as assembly."
               (printf "assembly\n")]
              
                            #|
              #:once-any
              [("--verbose" "-v") ("This is something."
                                   "Another line"
                                   "And another") 
               (void)]
              [("--quiet" "-q") ("This is something."
                                 "Another line"
                                 "And another") 
               (void)]
              #:once-each
              [("--verbose" "-v") ("This is something."
                                   "Another line"
                                   "And another") 
               (void)]
              [("--quiet" "-q") ("This is something."
                                 "Another line"
                                 "And another") 
               (void)]
              #:usage-help "usage help" "line 2"
              #:help-labels "help labels" "line deux"
              #:ps "ps" "ps2"
              #:args (test) test
              |#

              )

; put this handler after because `command-line` takes care
; of its own error reporting
(uncaught-exception-handler
 (λ (e)
   (displayln "jade internal error:")
   (cond
     [(exn? e)
      (displayln (exn-message e))]
     [else
      (displayln e)])
   (exit 255)))
