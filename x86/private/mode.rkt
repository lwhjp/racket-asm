#lang racket/base

(require racket/contract)

(provide (all-defined-out))

(define default-assembler-bits
  (let ([word-size (system-type 'word)])
    (if (memv word-size '(16 32 64))
        word-size
        64)))

(define/contract current-assembler-bits
  (parameter/c (or/c 16 32 64))
  (make-parameter default-assembler-bits))
