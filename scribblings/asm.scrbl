#lang scribble/manual

@title{Racket Assembler}

@centered{@bold{Warning}}

This experimental package generates executable machine code.
It is incomplete, mostly untested and contains bugs. You will
not get a nice error message when things go wrong; if you are
lucky, the Racket process will crash.

@italic{Caveat usor.}

This project uses some ideas from Noel Welsh's
@hyperlink["https://github.com/noelwelsh/assembler/"]{x86 assembler},
and
@hyperlink["https://www.gnu.org/software/lightning/"]{GNU lightning}.

See the example directory for demos.

@(table-of-contents)

@include-section["assembler.scrbl"]
@include-section["ffi.scrbl"]
@include-section["generic.scrbl"]
@include-section["x86.scrbl"]
