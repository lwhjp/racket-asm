#lang racket

;;
;; Translated from the GNU lightning RPN example
;;

(require asm
         asm/generic
         ffi/unsafe)

(provide compile-rpn)

(define (compile-rpn expr)
  (define data-size 8)
  (define (stack-push reg sp)
    (stx sp fp reg)
    (+ sp data-size))
  (define (stack-pop reg sp)
    (let ([sp (- sp data-size)])
      (ldx reg fp sp)
      sp))
  (define fn
    (assemble
     (prolog)
     (define in (arg 0))
     (define stack-base
       (alloca (* 32 data-size)))
     (define stack-ptr stack-base)
     (getarg r2 in)
     (let loop ([i 0])
       (cond
         [(= i (string-length expr)) (void)]
         [(regexp-match #px"^[0-9]+" expr i)
          =>
          (λ (m)
            (set! stack-ptr (stack-push r0 stack-ptr))
            (mov r0 (string->number (car m)))
            (loop (+ i (string-length (car m)))))]
         [(char=? #\x (string-ref expr i))
          (set! stack-ptr (stack-push r0 stack-ptr))
          (mov r0 r2)
          (loop (add1 i))]
         [else
          (set! stack-ptr (stack-pop r1 stack-ptr))
          (case (string-ref expr i)
            [(#\+) (add r0 r1 r0)]
            [(#\-) (sub r0 r1 r0)]
            [(#\*) (mul r0 r1 r0)]
            [(#\/) (div r0 r1 r0)]
            [else (error "cannot compile:" (substring expr i))])
          (loop (add1 i))]))
     (epilog)
     (ret r0)))
  (bytes->proc fn (_fun _int -> _int)))

(define c->f
  (compile-rpn "32x9*5/+"))

(define f->c
  (compile-rpn "x32-5*9/"))

#;(module+ main
  (printf "C\tF\n")
  (for ([i (in-range 0 101 10)])
    (printf "~a\t~a\n" i (c->f i)))
  (printf "\nF\tC\n")
  (for ([i (in-range 32 213 18)])
    (printf "~a\t~a\n" i (f->c i))))