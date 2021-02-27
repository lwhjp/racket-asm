#lang racket/base

(require racket/port
         "private/decode.rkt"
         "private/x86.rkt")

(provide (all-defined-out))

(define read-instruction (make-read-instruction %instruction-spec))

(define (disassemble [in (current-input-port)])
  (port->list read-instruction in))
