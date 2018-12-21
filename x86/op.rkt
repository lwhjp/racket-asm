#lang racket/base

(require "private/encode.rkt"
         "private/x86.rkt")

(provide (all-defined-out))

(define-instruction-encoders %instruction-spec instruction-arity-map)

(define jc jb)
(define jnae jb)
(define jnc jnb)
(define jae jnb)
(define je jz)
(define jne jnz)
(define jna jbe)
(define ja jnbe)
(define jpe jp)
(define jpo jnp)
(define jnge jl)
(define jge jnl)
(define jng jle)
(define jg jnle)

(define (nop)
  (local-require "private/mode.rkt" "register.rkt")
  (case (current-assembler-bits)
    [(16) (xchg ax ax)]
    [(32 64) (xchg eax eax)]))

(define setc setb)
(define setnae setb)
(define setnc setnb)
(define setae setnb)
(define sete setz)
(define setne setnz)
(define setna setbe)
(define seta setnbe)
(define setpe setp)
(define setpo setnp)
(define setnge setl)
(define setge setnl)
(define setng setle)
(define setg setnle)
