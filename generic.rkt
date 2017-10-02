#lang racket/base

(provide (all-defined-out))

(require racket/unit
         "generic/generic-asm-sig.rkt"
         "generic/generic-x86-unit.rkt")

(define-values/invoke-unit generic-x86@
  (import)
  (export generic-asm^))
