#lang info
(define collection "asm")
(define build-deps '("racket-doc" "rackunit-lib" "scribble-lib"))
(define deps '("base" "binutils" "data-lib" "racklog"))
(define scribblings '(("scribblings/asm.scrbl" (multi-page))))
(define compile-omit-paths '("example/"))
