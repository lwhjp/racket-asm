#lang racket/base

(provide (all-defined-out))

(define (immediate? v)
  (exact-integer? v))

(define (immediate-width v)
  (integer-length v))
