#lang scribble/manual
@(require (for-label racket/base
                     ffi/unsafe
                     asm/base
                     asm/ffi))

@title{FFI Utilities}

@defmodule[asm/ffi]

@defproc[(alloc/exec [size exact-integer?]) bytes?]{
  Allocates @racket[size] bytes of executable memory.
}

@defproc[(bytes->proc [bs bytes?] [type ctype?]) procedure?]{
  Converts the assembled machine code in @racket[bs] to a
  native procedure with type @racket[type].
}

@defproc[(object->proc [obj ao:object?] [type ctype?]) procedure?]{
  Converts the assembled object @racket[obj] to a native
  procedure with type @racket[type].
}