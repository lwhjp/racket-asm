#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     racket/port
                     asm/x86/disasm))

@title{x86}

@section{Disassembly}

@defmodule[asm/x86/disasm]

@defproc[(read-instruction [in input-port? (current-input-port)])
         (list/c (listof byte?) list?)]{
Reads a single instruction from @racket[in] and returns a two-element
list containing the instruction bytes and disassembled expression.
}

@defproc[(disassemble [in input-port? (current-input-port)])
         (listof (list/c (listof byte?) list?))]{
Disassembles @racket[in], equivalent to
@racketblock[
(port->list read-instruction in)
]
}

@section{Instructions}

@defmodule[asm/x86]{
  Instructions for 64-bit x86 processors.
}
