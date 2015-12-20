#lang racket

(provide (all-defined-out))

(define (ndisasm bstr)
  (parameterize
      ([current-input-port (open-input-bytes bstr)]
       [current-output-port (open-output-string)])
    (and
     (system* "/usr/bin/ndisasm" "-b64" "-")
     (get-output-string (current-output-port)))))

(module+ test
  (require rackunit)
  (define code
    (bytes #xB8 #x2A #x00 #x00 #x00 #xC3))
  (define expected
    #<<END
00000000  B82A000000        mov eax,0x2a
00000005  C3                ret

END
    )
  (check-equal? (ndisasm code) expected))