#lang scribble/manual
@(require (for-label racket/base
                     asm/base))

@title{Assembler}

@defmodule[asm/base]

@defform[(assemble form-or-label ...)
         #:grammar
         [(form-or-label form
                         (code:line #:global global-id)
                         (code:line #:label label-id))]]{
  Evaluates each @racket[form] similarly to a @racket[begin]
  block. @racket[global-id] and @racket[label-id] declare global
  and local symbols respectively.
  Returns an @racket[ao:object?] containing assembled machine code from
  any evaluated instructions.
}
