#lang racket/base
(require racket/match
         (only-in racket/string string-join)
         "../static/record.rkt"
         "../static/object.rkt"
         "../error.rkt"
         "base.rkt"
         "uint.rkt"
         "bytes.rkt"
         "instruction.rkt"
         "../assembly.rkt")

(define pragma-directive
  (>> (literal "#pragma ")
      (lift (λ (cs) (pragma [content (apply string cs)])) (p* (p- (cc void) line-sentinel)))))

(module+ test
  (parse-success pragma-directive
                 "#pragma hello"
                 (pragma [content "hello"]))

  (parse-failure pragma-directive
                 "#pragmahello"
                 #rx""))

(define comment
  (>> (literal "//")
      (lift (λ (cs) (apply string cs)) (p* (p- (cc void) line-sentinel)))))


(module+ test
  (parse-success (>> whitespace*
                     (>>0 (p? byte-parser)
                          whitespace*
                          (p? comment)
                          line-sentinel))
                 "\tbyte base64 ZWE=\n"
                 (bytes-immediate [value #"ea"]))

  (parse-success (>> whitespace*
                     (>>0 (p? byte-parser)
                          whitespace*
                          (p? comment)
                          line-sentinel))
                 "\tbyte base64 ZWE= // this is a comment\n"
                 (bytes-immediate [value #"ea"])))

(define label-declaration
  (>>0 (>>= label-identifier (λ (ℓ) (unit (label ℓ))))
       space*
       (literal ":")))

(define int-parser
  (make-instruction-parser "int" (λ (value) (varuint-immediate value)) guarded-varuint))

(module+ test
  (parse-success int-parser
                 "int 25"
                 (varuint-immediate [value 25])))

(define byte-parser
  (make-instruction-parser "byte" (λ (value) (bytes-immediate value)) guarded-bytes))

(module+ test
  (parse-success byte-parser
                 "byte base64 ZWE="
                 (bytes-immediate [value #"ea"])))

(define (parse input)
  ((>> whitespace* (>>0 pragma-directive line-sentinel))
   input 0
   (λ (xs i fk)
     (match xs
       [(list (pragma content))
        (match (regexp-match #px"^version (\\d+)$" content)
          [(list _ (app string->number lsv))
           (let* ([directive (∨ (∨ (>>= (instruction-parser/version lsv) (λ (instr) (unit (instruction [instruction instr]))))
                                   int-parser
                                   byte-parser)
                                pragma-directive
                                label-declaration)]
                  [line (>> whitespace*
                            (>>0 (p? directive)
                                 whitespace*
                                 (p? comment)
                                 line-sentinel))])
             (define (loop)
               (∨ (>> whitespace* end-of-input (unit (list)))
                  (>>= line (λ (directive)
                              (if directive
                                (>>= (loop)
                                     (λ (directives)
                                       (unit (cons directive directives))))
                                (loop))))))
             (match ((loop)
                     input i
                     (λ (xs i fk) (match xs [(list x) x]))
                     (λ ()
                       (error 'bad-directive
                              (report input i
                                      "an instruction"
                                      "a comment signalled by //"
                                      "a #pragma directive"))))
               [(error-result tag message)
                (error-result tag message)]
               [directives
                (assembly [logic-sig-version lsv] directives)]))]
          [#f
           (error 'teal-version-missing (report input 0 "#pragma version <teal-version>"))])]))
   (λ () (error 'teal-version-missing (report input 0 "#pragma version <teal-version>")))))

(provide parse)

(module+ main
  (require racket/port
           racket/pretty
           "../assembly/show.rkt"
           "../assembly/control.rkt")

  (let ([input (port->string (current-input-port))])
    (displayln input)
    (let ([asm (time (parse input))])
      (displayln (assembly-show asm))
      (pretty-print (control-flow-graph asm)))))
