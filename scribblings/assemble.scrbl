#lang scribble/manual
@(require (for-label racket/base
                     "../assemble.rkt"))

@title{Assembler}

@declare-exporting[asm/assemble]

@defform[(assemble form-or-label ...)
         #:grammar
         [(form-or-label form
                         (code:line #:label label-id))]]{
  Evaluates each @racket[form] similarly to a @racket[begin]
  block, with each @racket[label-id] bound to a @racket[label].
  Returns a byte string containing assembled machine code from
  any evaluated instructions.
}