#lang racket

(require
  "../object.rkt"
  "private/define-instruction.rkt"
  "private/instruction.rkt"
  "reference.rkt"
  "register.rkt")

(provide
 (all-defined-out))

(module+ test
  (require
    rackunit
    "../assemble.rkt")
  (define-check (check-generated-asm name actual expected)
    (let ([gen (ao:object-text actual)])
      (with-check-info*
       (list
        (make-check-actual gen)
        (make-check-expected expected))
       (λ ()
         (unless (equal? gen expected)
           (fail-check))))))
  (define-syntax-rule (check-asm actual expected)
    (check-generated-asm 'check-asm actual expected))
  (define-syntax-rule (check-instruction ins expected)
    (check-generated-asm 'check-instruction (assemble ins) expected)))

(define-values (adc add sbb sub)
  (let ([make-op
         (λ (add? carry?)
           (λ (v w)
             (cond
               [(and (reg/mem? v) (not (reg/mem? w)))
                (define sub-op
                  (match* (add? carry?)
                    [(#t #t) 2]
                    [(#t #f) 0]
                    [(#f #t) 3]
                    [(#f #f) 5]))
                (define 8bit? (eqv? 8 (r/m-width v)))
                (make-instruction (if 8bit? #x80 #x81)
                                  #:reg sub-op
                                  #:r/m v
                                  #:immediate w
                                  #:default-operand-size (if 8bit? 8 32))]
               [(and (reg/mem? v) (register? w))
                (define-values (op8 op)
                  (match* (add? carry?)
                    [(#t #t) (values #x10 #x11)]
                    [(#t #f) (values #x00 #x01)]
                    [(#f #t) (values #x18 #x19)]
                    [(#f #f) (values #x28 #x29)]))
                (define 8bit? (eqv? 8 (register-width w)))
                (make-instruction (if 8bit? op8 op)
                                  #:reg w
                                  #:r/m v
                                  #:default-operand-size (if 8bit? 8 32))]
               [(and (register? v) (reg/mem? w))
                (define-values (op8 op)
                  (match* (add? carry?)
                    [(#t #t) (values #x12 #x13)]
                    [(#t #f) (values #x02 #x03)]
                    [(#f #t) (values #x1A #x1B)]
                    [(#f #f) (values #x2A #x2B)]))
                (define 8bit? (eqv? 8 (register-width v)))
                (make-instruction (if 8bit? op8 op)
                                  #:reg v
                                  #:r/m w
                                  #:default-operand-size (if 8bit? 8 32))]
               [else (error "illegal operands")])))])
    (values (make-op #t #t)
            (make-op #t #f)
            (make-op #f #t)
            (make-op #f #f))))

(define-instruction call
  [(rel16off) (op "E8 iw")]
  [(rel32off) (op "E8 id")]
  [(reg/mem16) (op "FF /2")]
  #;[(reg/mem32) (op "FF /2")]
  [(reg/mem64) (op "FF /2" #:default-operand-size 64)])

(define-instruction cmp
  #;[(AL imm8) (op "3C ib")]
  #;[(AX imm16) (op "3D iw")]
  #;[(EAX imm32) (op "3D id")]
  #;[(RAX imm32) (op "3D id")]
  [(reg/mem8 imm8) (op "80 /7 ib")]
  [(reg/mem16 imm8) (op "83 /7 ib")]
  [(reg/mem32 imm8) (op "83 /7 ib")]
  [(reg/mem64 imm8) (op "83 /7 ib")]
  [(reg/mem16 imm16) (op "81 /7 iw")]
  [(reg/mem32 imm32) (op "81 /7 id")]
  [(reg/mem64 imm32) (op "81 /7 id")]
  [(reg/mem8 reg8) (op "38 /r")]
  [(reg/mem16 reg16) (op "39 /r")]
  [(reg/mem32 reg32) (op "39 /r")]
  [(reg/mem64 reg64) (op "49 /r")]
  [(reg8 reg/mem8) (op "3A /r")]
  [(reg16 reg/mem16) (op "3B /r")]
  [(reg32 reg/mem32) (op "3B /r")]
  [(reg64 reg/mem64) (op "3B /r")])

(module+ test
  #;(check-instruction (cmp al 1) (bytes #x3C #x01))
  #;(check-instruction (cmp eax 1000) (bytes #x3D #xE8 #x03 #x00 #x00))
  (check-instruction (cmp bh 1) (bytes #x80 #xFF #x01))
  (check-instruction (cmp ebx 1000) (bytes #x81 #xFB #xE8 #x03 #x00 #x00))
  (check-instruction (cmp ebx 1) (bytes #x83 #xFB #x01))
  (check-instruction (cmp (ptr ebx) cl) (bytes #x67 #x38 #x0B))
  (check-instruction (cmp (ptr rbx) cl) (bytes #x38 #x0B))
  (check-instruction (cmp (ptr rbx) ecx) (bytes #x39 #x0B))
  (check-instruction (cmp bl (ptr rcx)) (bytes #x3A #x19))
  (check-instruction (cmp ebx (ptr rcx)) (bytes #x3B #x19)))

(define-instruction dec
  [(reg/mem8) (op "FE /1")]
  [(reg/mem16) (op "FF /1")]
  [(reg/mem32) (op "FF /1")]
  [(reg/mem64) (op "FF /1")]
  [(reg16) (op "48 +rw")]
  [(reg32) (op "48 +rd")])

(module+ test
  (check-instruction (dec al) (bytes #xFE #xC8))
  (check-instruction (dec ah) (bytes #xFE #xCC))
  (check-instruction (dec ax) (bytes #x66 #xFF #xC8))
  (check-instruction (dec rax) (bytes #x48 #xFF #xC8))
  (check-instruction (dec r10b) (bytes #x41 #xFE #xCA))
  (check-instruction (dec r10w) (bytes #x66 #x41 #xFF #xCA))
  (check-instruction (dec r10d) (bytes #x41 #xFF #xCA))
  (check-instruction (dec r10) (bytes #x49 #xFF #xCA)))

(define-instruction div
  [(reg/mem8) (op "F6 /6")]
  [(reg/mem16) (op "F7 /6")]
  [(reg/mem32) (op "F7 /6")]
  [(reg/mem64) (op "F7 /6")])

(module+ test
  (check-instruction (div bh) (bytes #xF6 #xF7))
  (check-instruction (div ebx) (bytes #xF7 #xF3)))

(define-instruction inc
  [(reg/mem8) (op "FE /0")]
  [(reg/mem16) (op "FF /0")]
  [(reg/mem32) (op "FF /0")]
  [(reg/mem64) (op "FF /0")]
  [(reg16) (op "40 +rw")]
  [(reg32) (op "40 +rd")])

(define-values (jo jno jb jnb jz jnz jbe jnbe js jns jp jnp jl jnl jle jnle)
  (apply
   values
   (build-list
    16
    (λ (i)
      (λ (u)
        (make-instruction (bytes #x0F (+ #x80 i))
                          #:immediate u))))))

(define jc jb)
(define jnae jb)
(define jnc jnb)
(define jae jnb)
(define je jz)
(define jne jnz)
(define jna jbe)
(define ja jnbe)
(define jpe jp)
(define jpo jnp)
(define jnge jl)
(define jge jnl)
(define jng jle)
(define jg jnle)

(define-instruction jmp
  [(rel8off) (op "EB cb")]
  [(rel16off) (op "E9 cw")]
  [(rel32off) (op "E9 cd")]
  [(reg/mem16) (op "FF /4")]
  #;[(reg/mem32) (op "FF /4")]
  [(reg/mem64) (op "FF /4")])

(module+ test
  (check-asm
   (assemble
    #:label foo
    (nop)
    (jmp 'foo))
   #;(bytes #x90 #xEB #xFD)
   (bytes #x90 #xE9 #xFA #xFF #xFF #xFF)))

(define-instruction mov
  [(reg/mem8 reg8) (op "88 /r")]
  [(reg/mem16 reg16) (op "89 /r")]
  [(reg/mem32 reg32) (op "89 /r")]
  [(reg/mem64 reg64) (op "89 /r")]
  [(reg8 reg/mem8) (op "8A /r")]
  [(reg16 reg/mem16) (op "8B /r")]
  [(reg32 reg/mem32) (op "8B /r")]
  [(reg64 reg/mem64) (op "8B /r")]
  ; TODO: 8C, 8E
  ; Not implemented: A0, A1, A2, A3
  [(reg8 imm8) (op "B0 +rb ib")]
  [(reg16 imm16) (op "B8 +rw iw")]
  [(reg32 imm32) (op "B8 +rd id")]
  [(reg64 imm64) (op "B8 +rq iq")]
  [(reg/mem8 imm8) (op "C6 /0 ib")]
  [(reg/mem16 imm16) (op "C7 /0 iw")]
  [(reg/mem32 imm32) (op "C7 /0 id")]
  [(reg/mem64 imm32) (op "C7 /0 id")])

(module+ test
  (check-instruction (mov ah ch) (bytes #x88 #xEC))
  (check-instruction (mov bx cx) (bytes #x66 #x89 #xCB))
  (check-instruction (mov esi edi) (bytes #x89 #xFE))
  (check-instruction (mov r8 r9) (bytes #x4D #x89 #xC8))
  (check-instruction (mov eax 42) (bytes #xB8 #x2A #x00 #x00 #x00))

  (check-instruction (mov rax (ptr rbx))
                     (bytes #x48 #x8B #x03))
  (check-instruction (mov rax (ptr ebx))
                     (bytes #x67 #x48 #x8B #x03))
  (check-instruction (mov eax (ptr rbx))
                     (bytes #x8B #x03))
  (check-instruction (mov eax (ptr ebx))
                     (bytes #x67 #x8B #x03))
  (check-instruction (mov eax (ptr+ ebx ecx))
                     (bytes #x67 #x8B #x04 #x0B))
  (check-instruction (mov eax (ptr+ (ptr* 2 ebx) ecx))
                     (bytes #x67 #x8B #x04 #x59))
  (check-instruction (mov eax (ptr+ (ptr* 2 ebx) ecx 4))
                     #;(bytes #x67 #x8B #x44 #x59 #x04)
                     (bytes #x67 #x8B #x84 #x59 #x04 #x00 #x00 #x00))
  (check-instruction (mov eax (ptr+ ecx 4))
                     #;(bytes #x67 #x8B #x41 #x04)
                     (bytes #x67 #x8B #x81 #x04 #x00 #x00 #x00))
  (check-instruction (mov eax (ptr+ (ptr* 2 ebx) 4))
                     #;(bytes #x67 #x8B #x44 #x1B #x04)
                     (bytes #x67 #x8B #x04 #x5D #x04 #x00 #x00 #x00))
  (check-instruction (mov eax (ptr 4))
                     (bytes #x8B #x04 #x25 #x04 #x00 #x00 #x00)))

(define-instruction mul
  [(reg/mem8) (op "F6 /4")]
  [(reg/mem16) (op "F7 /4")]
  [(reg/mem32) (op "F7 /4")]
  [(reg/mem64) (op "F7 /4")])

(define-instruction nop
  [() (op "90")]
  #;[(reg/mem16) (op "0F 1F /0")]
  #;[(reg/mem32) (op "0F 1F /0")]
  #;[(reg/mem64) (op "0F 1F /0")])

(module+ test
  (check-instruction (nop) (bytes #x90)))

(define-instruction pop
  [(reg16) (op "58 +rw")]
  #;[(reg32) (op "58 +rd")] ; not available in 64-bit mode
  [(reg64) (op "58 +rq" #:default-operand-size 64)]
  ;; TODO: specify memory size
  #;[(reg/mem16) (op "8F /0")]
  #;[(reg/mem32) (op "8F /0")] ; not available in 64-bit mode
  #;[(reg/mem64) (op "8F /0" #:default-operand-size 64)]
  ;; TODO: POP DS, ES, SS, FS, GS
  )

(module+ test
  #;(check-instruction (pop (ptr rax)) (bytes #x8F #x00))
  (check-instruction (pop ax) (bytes #x66 #x58))
  (check-instruction (pop rax) (bytes #x58)))

(define-instruction push
  [(reg16) (op "50 +rw")]
  #;[(reg32) (op "50 +rw")] ; not available in 64-bit mode
  [(reg64) (op "50 +rq" #:default-operand-size 64)]
  ;; TODO: specify memory size
  #;[(reg/mem16) (op "FF /6")]
  #;[(reg/mem32) (op "FF /6")] ; not available in 64-bit mode
  #;[(reg/mem64) (op "FF /6" #:default-operand-size 64)]
  [(imm8) (op "6A ib")]
  [(imm16) (op "68 iw")]
  #;[(imm32) (op "68 id")]
  [(imm32) (op "68 id")] ; spec says imm64 but that is wrong?
  ;; TODO: PUSH CS, SS, DS, ES, FS, GS
  )

(module+ test
  (check-instruction (push #x12345678) (bytes #x68 #x78 #x56 #x34 #x12)))

(define-instruction ret
  [() (op "C3")]
  [(imm16) (op "C2 iw")])

(module+ test
  (check-instruction (ret) (bytes #xC3))
  (check-instruction (ret #x1234) (bytes #xC2 #x34 #x12)))

(define-values (seto setno setb setnb setz setnz setbe setnbe sets setns setp setnp setl setnl setle setnle)
  (apply
   values
   (build-list
    16
    (λ (i)
      (λ (u)
        (make-instruction (bytes #x0F (+ #x90 i))
                          #:reg 0
                          #:r/m u
                          #:operand-size 8))))))

(define setc setb)
(define setnae setb)
(define setnc setnb)
(define setae setnb)
(define sete setz)
(define setne setnz)
(define setna setbe)
(define seta setnbe)
(define setpe setp)
(define setpo setnp)
(define setnge setl)
(define setge setnl)
(define setng setle)
(define setg setnle)
