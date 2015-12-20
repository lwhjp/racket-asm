#lang racket

(require
  ffi/unsafe
  "assemble.rkt")

(provide
 bytes->proc
 define/asm)

(define alloc/exec
  (get-ffi-obj
   'scheme_malloc_code
   #f
   (_fun (size : _long) -> (_bytes o size))))

(define (bytes->proc bs type)
  (let ([code (alloc/exec (bytes-length bs))])
    (bytes-copy! code 0 bs)
    (function-ptr code type)))

(define-syntax-rule
  (define/asm (id fun-spec ...)
    body ...)
  (define id
    (bytes->proc
     (object-code
      (assemble
       body ...))
     (_fun fun-spec ...))))