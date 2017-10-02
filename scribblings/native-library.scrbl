#lang scribble/manual
@(require (for-label racket/base
                     racket/class
                     ffi/unsafe
                     asm/base
                     "../native-library.rkt"))

@title{Native libraries}

@defmodule[asm/native-library]

@defclass[native-library% object% ()]{
  Allows linking assembled objects.
  @defconstructor[([size exact-integer?])]{
    @racket[size] specifies the number of bytes
    to reserve. Use @racket[make-native-library]
    for easy construction.
  }
  @defmethod[(load
              [objects (listof ao:object?)]
              [offset exact-integer? 0])
             void?]{
    Load the @racket[objects] into executable
    memory starting at offset @racket[offset].
    Symbol references are resolved from previously
    loaded objects.
  }
  @defmethod[(symbol-offset [name symbol?])
             (or/c exact-integer? #f)]{
    Get the address (as an offset from the start
    of this library's memory block) of the symbol
    @racket[name].
  }
  @defmethod[(symbol-pointer
              [name symbol?]
              [type (or/c ctype? #f) #f])
             (or/c cpointer? #f)]{
    Get a pointer to the symbol @racket[name],
    optionally with function type @racket[type].
  }
}

@defproc[(make-native-library
          [objects (listof ao:object?)])
         (is-a?/c native-library%)]{
  Create a @racket[native-library%] with the
  specified @racket[objects] pre-loaded.
}