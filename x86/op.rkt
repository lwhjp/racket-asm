#lang racket

(require
  "../types.rkt"
  "instruction.rkt"
  "reference.rkt"
  "register.rkt")

(provide
 (all-defined-out))

(module+ test
  (require
    rackunit
    "../assemble.rkt"))

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

(define (call v)
  (cond
    [(label? v)
     ; TODO: 8/16 bit offsets
     (make-instruction #xE8
                       #:immediate v)]
    [(reg/mem? v)
     (make-instruction #xFF
                       #:reg 2
                       #:r/m v
                       #:default-operand-size 64)]
    [else (error 'call "illegal argument: ~a" v)]))

(define (cmp v w)
  (cond
    [(and (reg/mem? v) (not (reg/mem? w)))
     (define 8bit? (eqv? 8 (r/m-width v)))
     (make-instruction (if 8bit? #x80 #x81)
                       #:reg 7
                       #:r/m v
                       #:immediate w
                       #:default-operand-size (if 8bit? 8 32))]
    [(and (reg/mem? v) (register? w))
     (define 8bit? (eqv? 8 (register-width w)))
     (make-instruction (if 8bit? #x38 #x39)
                       #:reg w
                       #:r/m v
                       #:default-operand-size (if 8bit? 8 32))]
    [(and (register? v) (reg/mem? w))
     (define 8bit? (eqv? 8 (register-width v)))
     (make-instruction (if 8bit? #x3A #x3B)
                       #:reg v
                       #:r/m w
                       #:default-operand-size (if 8bit? 8 32))]
    [else (error "illegal operands")]))

(define (dec v)
  (cond
    [(reg/mem? v)
     (define 8bit? (eqv? 8 (r/m-width v)))
     (make-instruction (if 8bit? #xFE #xFF)
                       #:reg 1
                       #:r/m v
                       #:default-operand-size (if 8bit? 8 32))]
    [(register? v)
     (make-instruction (+ #x48 (bitwise-and #x7 (register-code v)))
                       #:operand-size (register-width v))]
    [else (error "illegal argument")]))

(define (div v)
  (define 8bit? (eqv? 8 (r/m-width v)))
  (make-instruction (if 8bit? #xF6 #xF7)
                    #:reg 6
                    #:r/m v
                    #:default-operand-size (if 8bit? 8 32)))

(define (inc v)
  (cond
    [(reg/mem? v)
     (define 8bit? (eqv? 8 (r/m-width v)))
     (make-instruction (if 8bit? #xFE #xFF)
                       #:reg 0
                       #:r/m v
                       #:default-operand-size (if 8bit? 8 32))]
    [(register? v)
     (make-instruction (+ #x40 (bitwise-and #x7 (register-code v)))
                       #:operand-size (register-width v))]
    [else (error "illegal argument")]))

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

(define (jmp v)
  (cond
    [(label? v)
     ; TODO: 8/16 bit offsets
     (make-instruction #xE9
                       #:immediate v)]
    [(reg/mem? v)
     (make-instruction #xFF
                       #:reg 4
                       #:r/m v
                       #:default-operand-size 64)]
    [else (error 'jmp "illegal argument: ~a" v)]))

(module+ test
  (check-equal? (assemble
                 #:label foo
                 (nop)
                 (jmp foo))
                #;(bytes #x90 #xEB #xFD)
                (bytes #x90 #xE9 #xFA #xFF #xFF #xFF)))

(define (mov dest src)
  (cond
    [(and (reg/mem? dest) (register? src))
     (define 8bit? (eqv? 8 (register-width src)))
     (make-instruction (if 8bit? #x88 #x89)
                       #:reg src
                       #:r/m dest
                       #:default-operand-size (if 8bit? 8 32))]
    [(and (register? dest) (reg/mem? src))
     (define 8bit? (eqv? 8 (register-width dest)))
     (make-instruction (if 8bit? #x8A #x8B)
                       #:reg dest
                       #:r/m src
                       #:default-operand-size (if 8bit? 8 32))]
    ; TODO: 8C, 8E
    ; Not implemented: A0, A1, A2, A3
    [(and (register? dest) (not (reg/mem? src)))
     (make-instruction (+ (if (eqv? 8 (register-width dest)) #xB0 #xB8)
                          (bitwise-and #x7 (register-code dest)))
                       #:reg dest
                       #:immediate src
                       #:immediate-size (register-width dest))]
    [(and (reg/mem? dest) (not (reg/mem? src)))
     (make-instruction (if (eqv? 8 (r/m-width dest)) #xC6 #xC7)
                       #:reg 0
                       #:r/m dest
                       #:immediate src)]
    [else (error "illegal operands")]))

(define (mul v)
  (make-instruction (if (eqv? 8 (r/m-width v)) #xF6 #xF7)
                    #:reg 4
                    #:r/m v))

(module+ test
  (check-equal? (assemble (mov ah ch)) (bytes #x88 #xEC))
  (check-equal? (assemble (mov bx cx)) (bytes #x66 #x89 #xCB))
  (check-equal? (assemble (mov esi edi)) (bytes #x89 #xFE))
  (check-equal? (assemble (mov r8 r9)) (bytes #x4D #x89 #xC8))
  (check-equal? (assemble (mov eax 42)) (bytes #xB8 #x2A #x00 #x00 #x00))

  (check-equal? (assemble (mov rax (ptr rbx)))
                (bytes #x48 #x8B #x03))
  (check-equal? (assemble (mov rax (ptr ebx)))
                (bytes #x67 #x48 #x8B #x03))
  (check-equal? (assemble (mov eax (ptr rbx)))
                (bytes #x8B #x03))
  (check-equal? (assemble (mov eax (ptr ebx)))
                (bytes #x67 #x8B #x03))
  (check-equal? (assemble (mov eax (ptr+ ebx ecx)))
                (bytes #x67 #x8B #x04 #x0B))
  (check-equal? (assemble (mov eax (ptr+ (ptr* 2 ebx) ecx)))
                (bytes #x67 #x8B #x04 #x59))
  (check-equal? (assemble (mov eax (ptr+ (ptr* 2 ebx) ecx 4)))
                #;(bytes #x67 #x8B #x44 #x59 #x04)
                (bytes #x67 #x8B #x84 #x59 #x04 #x00 #x00 #x00))
  (check-equal? (assemble (mov eax (ptr+ ecx 4)))
                #;(bytes #x67 #x8B #x41 #x04)
                (bytes #x67 #x8B #x81 #x04 #x00 #x00 #x00))
  (check-equal? (assemble (mov eax (ptr+ (ptr* 2 ebx) 4)))
                #;(bytes #x67 #x8B #x44 #x1B #x04)
                (bytes #x67 #x8B #x04 #x5D #x04 #x00 #x00 #x00))
  (check-equal? (assemble (mov eax (ptr 4)))
                (bytes #x8B #x04 #x25 #x04 #x00 #x00 #x00)))

(define (nop)
  (make-instruction #x90))

(define (pop v)
  (cond
    [(reg/mem? v)
     (make-instruction #x8F
                       #:reg 0
                       #:r/m v
                       #:default-operand-size 64)]
    [else (error 'pop "illegal operand")]))

(define (push v)
  (cond
    ;; TODO: PUSH CS, SS, DS, ES, FS, GS
    [(reg/mem? v)
     (make-instruction #xFF
                       #:reg 6
                       #:r/m v
                       #:default-operand-size 64)]
    [(not (reg/mem? v))
     (make-instruction #x68
                       #:immediate v
                       #:default-operand-size 64
                       #:immediate-size 32)]
    [else (error 'push "illegal operand")]))

(define (ret)
  (make-instruction #xC3))

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
