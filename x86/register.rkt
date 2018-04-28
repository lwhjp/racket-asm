#lang racket/base

(require (for-syntax racket/base)
         "private/operand.rkt")

(provide
 (except-out
  (all-defined-out)
  define-general-register))

(define-syntax (define-general-register stx)
  (syntax-case stx ()
    [(_ code (id ...))
     (with-syntax ([(width ...) '(8 16 32 64)])
       #'(define-values (id ...)
           (values (general-register 'id width code) ...)))]))

(define-general-register 0 (al ax eax rax))
(define-general-register 1 (cl cx ecx rcx))
(define-general-register 2 (dl dx edx rdx))
(define-general-register 3 (bl bx ebx rbx))
(define-general-register 4 (spl sp esp rsp))
(define-general-register 5 (bpl bp ebp rbp))
(define-general-register 6 (sil si esi rsi))
(define-general-register 7 (dil di edi rdi))
(define-general-register 8 (r8b r8w r8d r8))
(define-general-register 9 (r9b r9w r9d r9))
(define-general-register 10 (r10b r10w r10d r10))
(define-general-register 11 (r11b r11w r11d r11))
(define-general-register 12 (r12b r12w r12d r12))
(define-general-register 13 (r13b r13w r13d r13))
(define-general-register 14 (r14b r14w r14d r14))
(define-general-register 15 (r15b r15w r15d r15))

(define ah (general-register 'ah 8 4))
(define ch (general-register 'ch 8 5))
(define dh (general-register 'dh 8 6))
(define bh (general-register 'bh 8 7))

(define cs (segment-register 'cs 16))
(define ds (segment-register 'ds 16))
(define es (segment-register 'es 16))
(define fs (segment-register 'fs 16))
(define gs (segment-register 'gs 16))
(define ss (segment-register 'ss 16))
