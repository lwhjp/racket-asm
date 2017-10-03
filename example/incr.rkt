#lang racket

;;
;; Translated from the GNU lightning incr example
;;

(require asm
         asm/generic
         ffi/unsafe)

(provide incr)

(define incr-obj
  (assemble
   (prolog)
   (define in (arg 0))
   (getarg r0 in)
   (add r0 r0 1)
   (epilog)
   (ret r0)))

(define incr
  (object->proc incr-obj (_fun _int -> _int)))
