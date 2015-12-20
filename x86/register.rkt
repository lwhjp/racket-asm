#lang racket

(provide
 (except-out
  (all-defined-out)
  define-register))

(struct register (name width code high-byte?) #:transparent)

(define-syntax (define-register stx)
  (syntax-case stx ()
    [(_ code (name ...))
     (with-syntax ([(width ...) '(8 16 32 64)])
       #'(define-values (name ...)
           (values
            (register 'name width code #f) ...)))]))

(define-register 0 (al ax eax rax))
(define-register 1 (cl cx ecx rcx))
(define-register 2 (dl dx edx rdx))
(define-register 3 (bl bx ebx rbx))
(define-register 4 (spl sp esp rsp))
(define-register 5 (bpl bp ebp rbp))
(define-register 6 (sil si esi rsi))
(define-register 7 (dil di edi rdi))
(define-register 8 (r8b r8w r8d r8))
(define-register 9 (r9b r9w r9d r9))
(define-register 10 (r10b r10w r10d r10))
(define-register 11 (r11b r11w r11d r11))
(define-register 12 (r12b r12w r12d r12))
(define-register 13 (r13b r13w r13d r13))
(define-register 14 (r14b r14w r14d r14))
(define-register 15 (r15b r15w r15d r15))

(define ah (register 'ah 8 0 #t))
(define ch (register 'ch 8 1 #t))
(define dh (register 'dh 8 2 #t))
(define bh (register 'bh 8 3 #t))

(define ip (register 'ip 16 'ip #f))
(define eip (register 'eip 32 'ip #f))
(define rip (register 'rip 64 'ip #f))