#lang racket/base
(require json
         net/base64)

(for-each
 (λ (id)
   (with-handlers ([exn:fail? void])
     (let ([old-path (build-path "raw" id)]
           [new-path (build-path "extracted" id)])
       (unless (file-exists? new-path)
         (let* ([json (call-with-input-file old-path read-json)]
                [json (hash-ref json 'params)]
                [json (hash-ref json 'approval-program)])
           (call-with-output-file new-path
             (λ (op) (write-bytes (base64-decode (string->bytes/utf-8 json)) op))))))))
 (directory-list "raw"))


