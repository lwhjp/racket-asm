#lang racket/base

(require racket/contract)

(provide (all-defined-out))

(define/contract current-assembler-bits
  (parameter/c (or/c 16 32 64))
  (make-parameter 64))
