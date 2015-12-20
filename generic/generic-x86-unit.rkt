#lang racket/unit

(require
  "generic-asm-sig.rkt"
  (prefix-in x86: "../x86.rkt"))

(import)
(export generic-asm^)

(define v0 x86:rbx)
(define v1 x86:r13)
(define v2 x86:r14)
(define v3 x86:r15)
(define r0 x86:rax)
(define r1 x86:r10)
(define r2 x86:r11)
(define r3 x86:r12)
(define f0 #f)
(define f1 #f)
(define f2 #f)
(define f3 #f)
(define f4 #f)
(define f5 #f)
(define fp x86:rbp)

(define callee-save-regs
  (list x86:r12
        x86:r13
        x86:r14
        x86:r15
        x86:rbx
        x86:rbp))

(define caller-save-regs
  (list ; Restoring RAX will obliterate the return value...
        ;x86:rax
        x86:r10
        x86:r11))

(define arg-regs
  (list x86:rdi
        x86:rsi
        x86:rdx
        x86:rcx
        x86:r8
        x86:r9))

;;XXX
(define TODO (λ a (error 'TODO)))

(define-values (add addx addc sub subx subc)
  (apply
   values
   (map
    (λ (op)
      (λ (u v w)
        (unless (eq? x86:rax u)
          (x86:push x86:rax))
        (unless (eq? x86:rdx u)
          (x86:push x86:rdx))
        (x86:push v)
        (x86:push w)
        (x86:pop x86:rdx)
        (x86:pop x86:rax)
        (op x86:rax x86:rdx)
        (unless (eq? x86:rax u)
          (x86:mov u x86:rax))
        (unless (eq? x86:rdx u)
          (x86:pop x86:rdx))
        (unless (eq? x86:rax u)
          (x86:pop x86:rax))))
    (list x86:add x86:adc x86:add x86:sub x86:sbb x86:sub))))

(define (rsb u v w)
  (sub u w v))

(define-values (mul div rem)
  (apply
   values
   (map
    (λ (op)
      (λ (u v w)
        (unless (eq? x86:rax u)
          (x86:push x86:rax))
        (unless (eq? x86:rdx u)
          (x86:push x86:rdx))
        (unless (eq? x86:rbx u)
          (x86:push x86:rbx))
        (define out
          (op v w))
        (unless (eq? out u)
          (x86:mov u out))
        (unless (eq? x86:rbx u)
          (x86:pop x86:rbx))
        (unless (eq? x86:rdx u)
          (x86:pop x86:rdx))
        (unless (eq? x86:rax u)
          (x86:pop x86:rax))))
    (list (λ (v w)
            (x86:push v)
            (x86:push w)
            (x86:pop x86:rbx)
            (x86:pop x86:rax)
            (x86:mul x86:rbx)
            x86:rax)
          (λ (v w)
            (x86:push v)
            (x86:push w)
            (x86:pop x86:rbx)
            (x86:pop x86:rax)
            (x86:mov x86:rdx 0)
            (x86:div x86:rbx)
            x86:rax)
          (λ (v w)
            (x86:push v)
            (x86:push w)
            (x86:pop x86:rbx)
            (x86:pop x86:rax)
            (x86:mov x86:rdx 0)
            (x86:div x86:rbx)
            x86:rdx)))))

(define (and u v w)
  (TODO))

(define (or u v w)
  (TODO))

(define (xor u v w)
  (TODO))

(define (lsh u v w)
  (TODO))

(define (rsh u v w)
  (TODO))

(define (qmul l h v w)
  (TODO))

(define (qdiv l h v w)
  (TODO))

(define (neg u v)
  (TODO))

(define (com u v)
  (TODO))

(define (abs u v)
  (TODO))

(define (sqrt u v)
  (TODO))

(define-values (lt le gt ge eq ne unlt unle ungt unge)
  (apply
   values
   (map
    (λ (op)
      (λ (u v w)
        (x86:cmp v w)
        (op u)))
    (list x86:setb x86:setbe x86:seta x86:setae x86:sete
          x86:setne x86:setnae x86:setna x86:setnbe x86:setnb))))

(define uneq TODO)
(define ltgt TODO)
(define ord TODO)
(define unord TODO)

(define (mov u v)
  (x86:mov u v))

(define (ext u v)
  (TODO))

(define (trunc u v)
  (TODO))

(define (hton u v)
  (TODO))

(define (ntoh u v)
  (TODO))

(define (ld u v)
  (x86:mov u (x86:ptr v)))

(define (ldx u v w)
  (x86:mov u (x86:ptr+ v w)))

(define (st u v)
  (x86:mov (x86:ptr u) v))

(define (stx u v w)
  (x86:mov (x86:ptr+ u v) w))

(define-values (blt ble bgt bge beq bne bunlt bunle bungt bunge)
  (apply
   values
   (map
    (λ (op)
      (λ (u v w)
        (x86:cmp v w)
        (op u)))
    (list x86:jb x86:jbe x86:ja x86:jae x86:je
          x86:jne x86:jnae x86:jna x86:jnbe x86:jnb))))

(define-values (buneq bltgt bord bunord)
  (apply
   values
   (build-list
    4
    (λ (i)
      (λ (u v w)
        (TODO))))))

(define-values (bms bmc boadd bxadd bosub bxsub)
  (apply
   values
   (build-list
    6
    (λ (i)
      (λ (u v w)
        (TODO))))))

(define (prepare)
  (for-each x86:push callee-save-regs))

(define (call u)
  (x86:call u)
  (for-each x86:pop (reverse caller-save-regs)))

(define (finish u)
  ;; Hmm... ?
  (call u))

(define (jmp u)
  (TODO))

(define (retval u)
  (x86:mov u x86:rax))

(define (arg n)
  (cond
    [(< n 6) (list-ref arg-regs n)]
    [else (error 'arg "invalid arg number")]))

(define (getarg u v)
  (x86:mov u v))

(define (putarg u v)
  (x86:mov u v))

(define (prolog)
  (for-each x86:push callee-save-regs)
  (x86:mov x86:rbp x86:rsp))

(define (epilog)
  (x86:mov x86:rsp x86:rbp)
  (for-each x86:pop (reverse callee-save-regs)))

(define (alloca u)
  (x86:sub x86:rsp u)
  (- u))

(define ret
  (case-lambda
    [() (x86:ret)]
    [(u) (unless (eq? x86:rax u)
           (x86:mov x86:rax u))
         (x86:ret)]))

(define (frame u)
  (TODO))

(define (tramp u)
  (TODO))
