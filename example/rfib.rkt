#lang racket

;;
;; Translated from the GNU lightning Fibonacci example
;;

(require asm
         asm/generic
         ffi/unsafe
         "../ffi.rkt"
         "../object.rkt")

(provide rfib)

(define rfib-obj
  (assemble
   #:label top
   (prolog)
   (getarg v0 (arg 0))
   (blt 'end v0 2)
   (sub v1 v0 1)
   (sub v2 v0 2)
   (prepare)
   (putarg (arg 0) v1)
   (finish 'top)
   (retval v1)
   (prepare)
   (putarg (arg 0) v2)
   (finish 'top)
   (retval v2)
   (add v1 v1 1)
   (add r0 v1 v2)
   (epilog)
   (ret r0)
   #:label end
   (mov r0 1)
   (epilog)
   (ret r0)))

(define rfib
  (object->proc rfib-obj (_fun _int -> _int)))

(module+ main
  (require asm/x86/ndisasm)
  (display (ndisasm (ao:object-text rfib-obj))))