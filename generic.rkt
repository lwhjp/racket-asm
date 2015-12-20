#lang racket

(require
  "generic/generic-asm-sig.rkt"
  "generic/generic-x86-unit.rkt")

(provide
 (all-defined-out))

(define-values/invoke-unit generic-x86@
  (import)
  (export generic-asm^))