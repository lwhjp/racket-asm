#lang racket/base

(provide (all-defined-out))

(define (ao:symbol-binding? v)
  (and
   (memq v '(local global))
   #t))

(define (ao:reference-type? v)
  (and
   (memq v '(copy address relative))
   #t))

(struct ao:symbol
  (name
   value
   binding))

(struct ao:reference
  (symbol
   offset
   size
   addend
   type))

(struct ao:object
  (symbols
   references
   text))
