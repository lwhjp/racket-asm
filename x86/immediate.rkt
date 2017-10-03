#lang racket/base

(provide (all-defined-out))

(define (immediate? v)
  (or
   (exact-integer? v)
   (symbol? v)))

(define (immediate-width v)
  (if (exact-integer? v)
      (integer-length v)
      64)) ; FIXME
