#lang racket

(require
  ffi/unsafe
  "assemble.rkt"
  "object.rkt")

(provide
 bytes->proc
 object->proc
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

(define (object->proc obj type)
  (unless (zero? (vector-length (ao:object-references obj)))
    (error 'object->proc "object has unresolved references"))
  (bytes->proc (ao:object-text obj) type))

(define-syntax-rule
  (define/asm (id fun-spec ...)
    body ...)
  (define id
    (object->proc
     (assemble
      body ...)
     (_fun fun-spec ...))))