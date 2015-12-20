#lang scribble/manual
@(require (for-label racket/base
                     ffi/unsafe
                     "../ffi.rkt"))

@title{FFI Utilities}

@declare-exporting[asm/ffi]

@defproc[(bytes->proc [bs bytes?] [type ctype?]) procedure?]{
  Converts the assembled machine code in @racket[bs] to a
  native procedure with type @racket[type].
}