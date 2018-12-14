#lang racket/base

(require racklog
         "operand.rkt")

(provide (all-defined-out))

(define %general-register
  (let ([regs '((0 al ax eax rax)
                (3 bl bx ebx rbx)
                (1 cl cx ecx rcx)
                (2 dl dx edx rdx)
                (6 sil si esi rsi)
                (7 dil di edi rdi)
                (5 bpl bp ebp rbp)
                (4 spl sp esp rsp)
                (8 r8b r8w r8d r8)
                (9 r9b r9w r9d r9)
                (10 r10b r10w r10d r10)
                (11 r11b r11w r11d r11)
                (12 r12b r12w r12d r12)
                (13 r13b r13w r13d r13)
                (14 r14b r14w r14d r14)
                (15 r15b r15w r15d r15))])
    (%rel (name width code b w d q)
      [((general-register name width code))
       (%member (list code b w d q) regs)
       (%or (%and (%= name b) (%= width 8))
            (%and (%= name w) (%= width 16))
            (%and (%= name d) (%= width 32))
            (%and (%= name q) (%= width 64)))]
      [((general-register 'ah 8 4))]
      [((general-register 'bh 8 7))]
      [((general-register 'ch 8 5))]
      [((general-register 'dh 8 6))])))

(define %segment-register
  (%rel ()
    [((segment-register 'cs 16 1))]
    [((segment-register 'ds 16 3))]
    [((segment-register 'es 16 0))]
    [((segment-register 'fs 16 4))]
    [((segment-register 'gs 16 5))]
    [((segment-register 'ss 16 2))]))

(define %register-mode
  (%rel (r n w c)
    [((general-register n w c) 64)]
    [(r 32) (%= r (general-register n w c))
            (%member w '(8 16 32))
            (%register-with-rex (general-register n w c) #f)]
    [(r 16) (%register-mode r 32)]))

(define %register-with-rex
  (%rel (n w c)
    [((general-register n w c) (_)) (%member w '(16 32 64))
                                    (%member c '(0 1 2 3 4 5 6 7))]
    [((general-register n w c) #t) (%member c '(8 9 10 11 12 13 14 15))]
    [((general-register n 8 c) #f) (%member (cons c n)
                                            '((4 . ah)
                                              (7 . bh)
                                              (5 . ch)
                                              (6 . dh)))]
    [((general-register n 8 c) #t) (%member (cons c n)
                                            '((6 . sil)
                                              (7 . dil)
                                              (5 . bpl)
                                              (4 . spl)))]
    [((general-register n 8 c) (_)) (%member c '(0 1 2 3))]))
