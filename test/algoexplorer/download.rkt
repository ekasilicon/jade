#lang racket/base
(require racket/match
         net/url
         net/uri-codec
         racket/port)

(define nets
  (hash "mainnet" "https://algoindexer.algoexplorerapi.io/v2/applications"
        "testnet" "https://algoindexer.testnet.algoexplorerapi.io/v2/applications"
        "betanet" "https://algoindexer.betanet.algoexplorerapi.io/v2/applications"))

(define (applications #:limit limit #:next next)
  (get-pure-port (string->url (string-append (current-explorer-base)
                                             "?"
                                             (alist->form-urlencoded
                                              (append (if limit
                                                        (list (cons 'limit limit))
                                                        (list))
                                                      (if next
                                                        (list (cons 'next next))
                                                        (list))))))))

(define (application #:app-id app-id)
  (get-pure-port (string->url (string-append (current-explorer-base) "/" app-id))))

(define current-net-id (make-parameter #f))
(define current-explorer-base (make-parameter #f))

(module+ main
  #;
  (require racket/cmdline)
  #;
  (command-line
   #:program
   "algoexplorer-download"
   #:once-each
   [("-l" "--limit")
    limit
    "Number of programs to fetch per request"]
   [("–n" "--number")
    number
    "Number of requests to make"]
   )

  (require racket/pretty
           json)

  (define (save! app-id fetch-json)
    (define directory (current-net-id))
    (unless (directory-exists? directory)
      (make-directory directory))
    (let ([path (build-path directory app-id)])
      (if (file-exists? path)
        (printf "already have ~a\n" app-id)
        (begin
          (printf "fetching ~a..." app-id)
          (let ([json (fetch-json)])
            (printf "done\n")
            (printf "writing ~a..." app-id)
            (call-with-output-file path (λ (op) (write-json json op))))
          (printf "done\n")))))
  (match (current-command-line-arguments)
    [(vector net-id "index")
     (cond
       [(hash-ref nets net-id #f)
        => (λ (explorer-base)
             (define (save!* applications)
               (printf "Saving ~a applications...\n" (length applications))
               (for-each
                (λ (application)
                  (save! (number->string (hash-ref application 'id)) (λ () application)))
                applications))
             (parameterize ([current-net-id net-id]
                            [current-explorer-base explorer-base])
               (let loop ([next #f])
                 (match (read-json (applications #:limit "200" #:next next))
                   [(hash-table ('applications applications)
                                ('next-token next))
                    (save!* applications)
                    (loop next)]
                   [(hash-table ('applications 'null))
                    (void)]))
               (displayln "done indexing")))]
       [else
        (displayln "expected <net-id> as first argument; one of...")
        (for-each (λ (net-id) (printf "  ~a\n" net-id)) (hash-keys nets))])]
    [(vector net-id app-ids ...)
     (cond
       [(hash-ref nets net-id #f)
        => (λ (explorer-base)
             (parameterize ([current-net-id net-id]
                            [current-explorer-base explorer-base])
               (for-each
                (λ (app-id)
                  (save! app-id (λ () (read-json (application #:app-id app-id)))))
                app-ids)))]
       [else
        (displayln "expected <net-id> as first argument; one of...")
        (for-each (λ (net-id) (printf "  ~a\n" net-id)) (hash-keys nets))])]))
