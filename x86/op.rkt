#lang racket/base

(require "private/encode.rkt")

(provide (all-defined-out))

(define-instructions-from-opcode-map "private/instructions.rkt" primary-opcode-map)

(module+ test
  (require rackunit
           binutils/base
           "../private/assembler.rkt"
           "kernel.rkt"
           "register.rkt")
  (define-check (check-generated-asm name actual expected)
    (let ([gen (bin:section-data (car (bin:object-sections actual)))])
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

(module+ test
  (check-instruction (cmp al 1) (bytes #x3C #x01))
  (check-instruction (cmp eax 1000) (bytes #x3D #xE8 #x03 #x00 #x00))
  (check-instruction (cmp bh 1) (bytes #x80 #xFF #x01))
  (check-instruction (cmp ebx (dword 1000)) (bytes #x81 #xFB #xE8 #x03 #x00 #x00))
  (check-instruction (cmp ebx (byte 1)) (bytes #x83 #xFB #x01))
  (check-instruction (cmp (byte ptr ebx) cl) (bytes #x67 #x38 #x0B))
  (check-instruction (cmp (byte ptr rbx) cl) (bytes #x38 #x0B))
  (check-instruction (cmp (dword ptr rbx) ecx) (bytes #x39 #x0B))
  (check-instruction (cmp bl (byte ptr rcx)) (bytes #x3A #x19))
  (check-instruction (cmp ebx (dword ptr rcx)) (bytes #x3B #x19)))

(module+ test
  (check-instruction (dec al) (bytes #xFE #xC8))
  (check-instruction (dec ah) (bytes #xFE #xCC))
  (check-instruction (dec ax) (bytes #x66 #xFF #xC8))
  (check-instruction (dec rax) (bytes #x48 #xFF #xC8))
  (check-instruction (dec r10b) (bytes #x41 #xFE #xCA))
  (check-instruction (dec r10w) (bytes #x66 #x41 #xFF #xCA))
  (check-instruction (dec r10d) (bytes #x41 #xFF #xCA))
  (check-instruction (dec r10) (bytes #x49 #xFF #xCA)))

(module+ test
  (check-instruction (div bh) (bytes #xF6 #xF7))
  (check-instruction (div ebx) (bytes #xF7 #xF3)))

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

(module+ test
  (check-asm
   (assemble
    #:label foo
    (nop)
    (jmp 'foo))
   #;(bytes #x90 #xEB #xFD)
   (bytes #x90 #xE9 #xFA #xFF #xFF #xFF)))

(module+ test
  (check-instruction (mov ah ch) (bytes #x88 #xEC))
  (check-instruction (mov bx cx) (bytes #x66 #x89 #xCB))
  (check-instruction (mov esi edi) (bytes #x89 #xFE))
  (check-instruction (mov r8 r9) (bytes #x4D #x89 #xC8))
  (check-instruction (mov eax 42) (bytes #xB8 #x2A #x00 #x00 #x00))

  (check-instruction (mov rax (qword ptr rbx)) (bytes #x48 #x8B #x03))
  (check-instruction (mov rax (qword ptr ebx)) (bytes #x67 #x48 #x8B #x03))
  (check-instruction (mov eax (dword ptr rbx)) (bytes #x8B #x03))
  (check-instruction (mov eax (dword ptr ebx)) (bytes #x67 #x8B #x03))
  (check-instruction (mov eax (dword ptr ebx + ecx)) (bytes #x67 #x8B #x04 #x19))
  (check-instruction (mov eax (dword ptr 2 * ebx + ecx)) (bytes #x67 #x8B #x04 #x59))
  (check-instruction (mov eax (dword ptr 2 * ebx + ecx + 4)) (bytes #x67 #x8B #x44 #x59 #x04))
  (check-instruction (mov eax (dword ptr ecx + 4)) (bytes #x67 #x8B #x41 #x04))
  (check-instruction (mov eax (dword ptr ebx + ebx + 4)) (bytes #x67 #x8B #x44 #x1B #x04))
  (check-instruction (mov eax (dword ptr 4)) (bytes #x8B #x04 #x25 #x04 #x00 #x00 #x00)))

(define (nop)
  (local-require "private/mode.rkt" "register.rkt")
  (case (current-assembler-bits)
    [(16) (xchg ax ax)]
    [(32 64) (xchg eax eax)]))

(module+ test
  (check-instruction (nop) (bytes #x90)))

(module+ test
  (check-instruction (pop (qword ptr rax)) (bytes #x8F #x00))
  (check-instruction (pop ax) (bytes #x66 #x58))
  (check-instruction (pop rax) (bytes #x58)))

(module+ test
  (check-instruction (push r12) (bytes #x41 #x54))
  (check-instruction (push #x12345678) (bytes #x68 #x78 #x56 #x34 #x12)))

(module+ test
  (check-instruction (ret) (bytes #xC3))
  (check-instruction (ret #x1234) (bytes #xC2 #x34 #x12)))

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
