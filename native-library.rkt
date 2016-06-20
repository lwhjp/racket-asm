#lang racket/base

(require ffi/unsafe
         racket/class
         "ffi.rkt"
         "link.rkt"
         "object.rkt")

(define native-library%
  (class object%
    (init size)
    (super-new)
    (define core (alloc/exec size))
    (define base-addr (cast core _pointer _intptr))
    (define symbols (make-hasheq))
    (printf "Allocated ~a bytes at 0x~x\n" size base-addr)
    (define/public (load objects [offset 0])
      (define new-syms
        (link!
         core
         objects
         #:base base-addr
         #:offset offset
         #:symbols symbols))
      (for ([s (in-list new-syms)])
        (printf "\t[0x~x] ~a\n"
                (ao:symbol-value s)
                (ao:symbol-name s))
        (hash-set! symbols (ao:symbol-name s) s))
      (printf "Loaded ~a symbols\n" (length new-syms)))
    (define/public (symbol-offset name)
      (cond
        [(hash-ref symbols name #f)
         =>
         (λ (s)
           (- (ao:symbol-value s) base-addr))]
        [else #f]))
    (define/public (symbol-pointer name [type #f])
      (cond
        [(symbol-offset name)
         =>
         (λ (off)
           (let ([p (ptr-add core off)])
             (if type
                 (function-ptr p type)
                 p)))]
        [else #f]))))

(define (make-native-library objects)
  (let ([lib (make-object native-library%
               (for/sum ([o (in-list objects)])
                 (bytes-length (ao:object-text o))))])
    (send lib load objects)
    lib))

(module+ test
  (require (only-in "example/ifib.rkt" ifib-obj)
           (only-in "example/rfib.rkt" rfib-obj))
  (let ([lib (make-native-library (list ifib-obj rfib-obj))])
    (define ifib
      (send lib symbol-pointer 'ifib (_fun _int -> _int)))
    (define rfib
      (send lib symbol-pointer 'rfib (_fun _int -> _int)))
    #;(begin
      (printf "ifib 36 = ~a\n" (ifib 36))
      (printf "rfib 36 = ~a\n" (rfib 36)))
    (void)))
