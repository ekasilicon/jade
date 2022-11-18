#lang racket/base
(require racket/match
         (only-in racket/string string-join)
         "../static/record.rkt"
         "../static/object.rkt"
         "../jade-error.rkt"
         "base.rkt"
         "uint.rkt"
         "bytes.rkt"
         "instruction.rkt"
         "../assembly.rkt")

(define eolp
  (∨ (cp #\newline)
     (>> (cp #\return)
         (cp #\newline))))

(define pragma-directive
  (>> (litp "#pragma ")
      (fmap (λ (content) (pragma content)) (fmap list->string (⋆p (\\p charp eolp))))))

(module+ test
  (parse-success pragma-directive
                 "#pragma hello"
                 (pragma [content "hello"]))

  (parse-failure pragma-directive
                 "#pragmahello"
                 #rx""))

(define byte-parser
  (make-instruction-parser "byte" (λ (value) (bytes-immediate value)) guarded-bytes))

(module+ test
  (parse-success byte-parser
                 "byte base64 ZWE="
                 (bytes-immediate [value #"ea"])))

(define comment
  (>> (litp "//")
      (fmap list->string (⋆p (\\p charp eolp)))))

(module+ test
  (parse-success (>> space*
                     (>>0 byte-parser
                          space*
                          (?p comment)
                          eolp))
                 "\tbyte base64 ZWE=\n"
                 (bytes-immediate [value #"ea"]))

  (parse-success (>> space*
                     (>>0 byte-parser
                          space*
                          (?p comment)
                          eolp))
                 "\tbyte base64 ZWE= // this is a comment\n"
                 (bytes-immediate [value #"ea"])))

(define int-parser
  (make-instruction-parser "int" (λ (value) (varuint-immediate value)) guarded-varuint))

(module+ test
  (parse-success int-parser
                 "int 25"
                 (varuint-immediate [value 25])))

(define label-declaration
  (>>0 (fmap (λ (ℓ) (label ℓ)) label-identifier)
       space*
       (litp ":")))

(define (parse input)
  ((>> space* (>>0 pragma-directive eolp))
   input 0
   (λ (xs i fk)
     (match xs
       [(list (pragma content))
        (match (regexp-match #px"^version (\\d+)$" content)
          [(list _ (app string->number lsv))
           (let ([directive (∨ (∨ (fmap (λ (instr) (instruction [instruction instr])) (instruction-parser/version lsv))
                                  int-parser
                                  byte-parser)
                               pragma-directive
                               label-declaration)])
             (define (loop)
               (∨ (>> space* (unit (list)))
                  (fmap cons
                        (>> space*
                            (>>0 directive
                                 space*
                                 (?p comment)
                                 (?p eolp)))
                        (delay (loop)))
                  (>> space*
                      (?p comment)
                      eolp
                      (delay (loop)))))
             ((loop)
              input i
              (λ (xs i fk)
                (if (= i (string-length input))
                  (match-let ([(list directives) xs])
                    (assembly [logic-sig-version lsv] directives))
                  (fk)))
              (λ ()
                (jade-error 'bad-directive
                            (report input i
                                    "an instruction"
                                    "a comment signalled by //"
                                    "a #pragma directive"))))
             )]
          [#f
           (jade-error 'teal-version-missing (report input 0 "#pragma version <teal-version>"))])]))
   (λ () (jade-error 'teal-version-missing (report input 0 "#pragma version <teal-version>")))))

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

