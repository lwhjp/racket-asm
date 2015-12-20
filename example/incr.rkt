#lang racket

;;
;; Translated from the GNU lightning incr example
;;

(require asm
         asm/generic
         ffi/unsafe)

(provide incr)

(define incr-code
  (assemble
   (prolog)
   (define in (arg 0))
   (getarg r0 in)
   (add r0 r0 1)
   (epilog)
   (ret r0)))

(define incr
  (bytes->proc incr-code (_fun _int -> _int)))

(module+ main
  (require asm/x86/ndisasm)
  (display (ndisasm incr-code)))