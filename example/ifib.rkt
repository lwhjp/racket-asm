#lang racket

;;
;; Translated from the GNU lightning Fibonacci example
;;

(require asm
         asm/generic
         ffi/unsafe)

(provide ifib
         ifib-obj)

(define ifib-obj
  (assemble
   #:global ifib
   (prolog)
   (define in (arg 0))
   (getarg r2 in)
   (mov r1 1)
   (blt 'end r2 2)
   (sub r2 r2 1)
   (mov r0 1)
   #:label loop
   (sub r2 r2 1)
   (add v0 r0 r1)
   (mov r0 r1)
   (add r1 v0 1)
   (bne 'loop r2 0)
   #:label end
   (mov r0 r1)
   (epilog)
   (ret r0)))

(define ifib
  (object->proc ifib-obj (_fun _int -> _int)))
