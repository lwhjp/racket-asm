#lang racket/base

(require "private/operand.rkt")

(provide
 register?
 register-name
 register-width
 general-register?
 segment-register?
 ptr
 byte
 word
 dword
 qword)
