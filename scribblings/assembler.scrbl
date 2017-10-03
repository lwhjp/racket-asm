#lang scribble/manual
@(require (for-label racket/base
                     asm/base
                     binutils/base))

@title{Assembler}

@defmodule[asm/base]

@defform[(assemble form-or-label ...)
         #:grammar
         [(form-or-label form
                         (code:line #:section section-id)
                         (code:line #:global global-id)
                         (code:line #:label label-id))]]{
  Evaluates each @racket[form] similarly to a @racket[begin]
  block. Each @racket[section-id] selects the section of the object
  to which the assembled bytes are appended. The default section
  is @racketid[|.text|].
  @racket[global-id] and @racket[label-id] declare global
  and local symbols respectively.
  Returns a @racket[bin:object?] containing assembled machine code from
  any evaluated instructions.
}
