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
      (if (file-exists? path)
        (printf "already have ~a\n" app-id)
        (begin
          (printf "downloading ~a..." app-id)
          (call-with-output-file path
            (λ (op) (copy-port (application app-id) op)))
          (printf "done\n")))))

  (for-each
   download!
   (vector->list (current-command-line-arguments))))
