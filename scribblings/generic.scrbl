#lang scribble/manual
@(require (for-label "../generic.rkt"))

@title{Generic Instructions}

@defmodule[asm/generic]{
  Based on the GNU lightning instruction set, with some differences.
}

@section{Registers}

@deftogether[(@defthing[v0 register?]
              @defthing[v1 register?]
              @defthing[v2 register?])]{
  Callee-saved registers.
}

@deftogether[(@defthing[r0 register?]
              @defthing[r1 register?]
              @defthing[r2 register?])]{
  Caller-saved registers.
}

@deftogether[(@defthing[f0 register?]
              @defthing[f1 register?]
              @defthing[f2 register?]
              @defthing[f3 register?]
              @defthing[f4 register?]
              @defthing[f5 register?])]{
  Floating-point registers.
}

@defthing[fp register?]{
  Frame pointer.
}

@section{Arithmetic}

@margin-note{
  Can any scribble experts think of a nice way of documenting
  all these functions without a load of typing?
}

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

@section{Comparisons}

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

@section{Branches}

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

@section{Function calls}

   ;; Function calls

   prepare
   pusharg
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
   tramp

@section{Other}

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
