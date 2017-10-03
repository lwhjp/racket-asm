#lang info
(define collection "asm")
(define build-deps '("racket-doc" "rackunit-lib" "scribble-lib"))
(define deps '("base" "data-lib" "git://github.com/lwhjp/racket-binutils"))
(define scribblings '(("scribblings/asm.scrbl" (multi-page))))
(define compile-omit-paths '("example/"))
