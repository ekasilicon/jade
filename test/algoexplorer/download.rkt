#lang racket/base
(require racket/match
         net/url
         racket/port)

(define nets
  (hash "mainnet" "https://algoexplorerapi.io/v2/applications/"
        "testnet" "https://testnet.algoexplorerapi.io/v2/applications/"))

(define current-net-id (make-parameter #f))
(define current-explorer-base (make-parameter #f))

(define (application app-id)
  (get-pure-port (string->url (string-append (current-explorer-base) app-id))))

(module+ main
  (define (download! app-id)
    (define directory (string-append (current-net-id) "-raw"))
    (unless (directory-exists? directory)
      (make-directory directory))
    (let ([path (build-path directory app-id)])
      (if (file-exists? path)
        (printf "already have ~a\n" app-id)
        (begin
          (printf "downloading ~a..." app-id)
          (call-with-output-file path
            (λ (op) (copy-port (application app-id) op)))
          (printf "done\n")))))
  (match (current-command-line-arguments)
    [(vector net-id app-ids ...)
     (cond
       [(hash-ref nets net-id #f)
        => (λ (explorer-base)
             (parameterize ([current-net-id net-id]
                            [current-explorer-base explorer-base])
               (for-each download! app-ids)))]
       [else
        (displayln "expected <net-id> as first argument; one of...")
        (for-each (λ (net-id) (printf "  ~a\n" net-id)) (hash-keys nets))])]))
