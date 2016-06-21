#lang scribble/manual
@(require (for-label racket/base
                     "../object.rkt"))

@title{Assembled objects}

@declare-exporting[asm/object]

@defstruct[ao:symbol
           ([name symbol?]
            [value exact-integer?]
            [binding (or/c 'local 'global)])]{
  Represents the location of a label from assembled code.
}

@defstruct[ao:reference
           ([symbol symbol?]
            [offset exact-integer?]
            [size exact-integer?]
            [addend exact-integer?]
            [type (or/c 'copy 'address 'relative)])]{
  Represents a placeholder for linking.
}

@defstruct[ao:object
           ([symbols (vectorof ao:symbol?)]
            [references (vectorof ao:reference?)]
            [text bytes?])]{
  Contains an assembled object.
}
