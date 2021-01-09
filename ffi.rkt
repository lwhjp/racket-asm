#lang racket/base

(provide
 alloc/exec
 bytes->proc
 object->proc
 define/asm)

(require binutils/base
         ffi/unsafe
         "private/assembler.rkt")

; FIXME: use binutils
(define alloc/exec
  (if (eq? 'racket (system-type 'vm))
      (get-ffi-obj 'scheme_malloc_code #f (_fun (size : _long) -> (_bytes o size)))
      ; TODO
      (λ (_) (error "dynamic loading is not implemented on this version of Racket"))))

(define (bytes->proc bs type)
  (let ([code (alloc/exec (bytes-length bs))])
    (bytes-copy! code 0 bs)
    (function-ptr code type)))

(define (object->proc obj type)
  (unless (eqv? 1 (length (bin:object-sections obj)))
    (error 'object->proc "unsupported: multi-section objects"))
  (define sec (car (bin:object-sections obj)))
  (unless (null? (bin:section-relocations sec))
    (error 'object->proc "object has unresolved relocations"))
  (bytes->proc (bin:section-data sec) type))

(define-syntax-rule
  (define/asm (id fun-spec ...)
    body ...)
  (define id
    (object->proc
     (assemble
      body ...)
     (_fun fun-spec ...))))