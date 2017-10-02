#lang racket/base

(provide
 generic-asm^)

(require racket/unit)

(define-signature generic-asm^
  (;; Callee-saved registers
   v0
   v1
   v2

   ;; Caller-saved registers
   r0
   r1
   r2

   ;; Floating-point registers
   f0
   f1
   f2
   f3
   f4
   f5

   ;; Frame pointer
   fp

   ;; Binary ALU operations
   add
   addx
   addc
   sub
   subx
   subc
   rsb
   mul
   div
   rem
   and
   or
   xor
   lsh
   rsh

   ;; Four operand binary ALU operations
   qmul
   qdiv

   ;; Unary ALU operations
   neg
   com
   abs
   sqrt

   ;; Compare instructions
   lt
   le
   gt
   ge
   eq
   ne
   unlt
   unle
   ungt
   unge
   uneq
   ltgt
   ord
   unord

   ;; Transfer operations
   mov
   ext
   trunc

   ;; Network extensions
   hton
   ntoh

   ;; Load operations
   ld
   ldx

   ;; Store operations
   st
   stx

   ;; Branch instructions
   blt
   ble
   bgt
   bge
   beq
   bne
   bunlt
   bunle
   bungt
   bunge
   buneq
   bltgt
   bord
   bunord
   bms
   bmc
   boadd
   bxadd
   bosub
   bxsub

   ;; Function calls
   prepare
   call
   finish
   jmp
   retval

   arg
   getarg
   putarg
   prolog
   epilog
   alloca
   ret
   frame
   tramp))