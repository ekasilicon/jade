#lang racket/base
(require net/url
         racket/port)

(define (application app-id)
  (get-pure-port (string->url (string-append
                                "https://algoexplorerapi.io/v2/applications/"
                                app-id))))

(module+ main
  (define (download! app-id)
    (let ([path (build-path "raw" app-id)])
      (unless (file-exists? path)
        (call-with-output-file path
          (λ (op) (copy-port (application app-id) op))))))

  (for-each
   download!
   '("297857417"
     "297989657"
     "297988505"
     "297987097"
     "297986082"
     "297985540"
     "297979678"
     "297976938"
     "297973877"
     "297971372"
     "297968089"
     "297967423"
     "297963833"
     "297961448"
     "297958692"
     "297949442"
     "297957819"
     "297948940"
     "297948938"
     "297946699"
     "297946183")))
