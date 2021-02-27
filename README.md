Racket Assembler
================

Installation
------------

Probably the easiest method is to create a local link using
`raco pkg install` in the top level of this repository.
This will create links so that `require` and friends work properly.

See the `example` directory for how to create executable procedures.

**NOTE** dynamic loading doesn't yet work on Racket CS (which is the
default from version 8.0). This will be addressed at some point.

Overview
--------

This is an assembler for Racket. The idea is to support both
specific processor instruction sets (x86-64 to begin with) and
a generic target in the style of GNU Lightning.

I used some ideas from Noel Welsh's
[x86 assembler](https://github.com/noelwelsh/assembler/).

My immediate goal is to implement enough to write a translator
from Racket bytecode to machine code, as an alternative module
loader. (The long-term "pie in the sky" vision is a self-hosted
Racket system).

Status
------

Some, but not all, x86-64 instructions are implemented.

Some, but not all, of the generic instruction set is implemented.

The instructions are buggy and are likely to crash your Racket process.

The examples (adapted from the GNU Lightning examples) run, but are
not perfect.

The generic instruction support needs some tweaking to be more
Racket-friendly and emit fewer useless instructions.