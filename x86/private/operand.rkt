#lang racket/base

(require (for-syntax racket/base)
         "../immediate.rkt"
         "../reference.rkt"
         "../register.rkt")

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-values (imm8? imm16? imm32? imm64?)
  (apply
   values
   (for/list ([width '(8 16 32 64)])
     (λ (v)
       (and (immediate? v)
            (<= (immediate-width v) width))))))

(define-values (reg8? reg16? reg32? reg64?)
  (apply
   values
   (for/list ([width '(8 16 32 64)])
     (λ (v)
       (and (register? v)
            (eqv? width (register-width v)))))))

(define-values (reg/mem8? reg/mem16? reg/mem32? reg/mem64?)
  (apply
   values
   (for/list ([reg-pred (list reg8? reg16? reg32? reg64?)])
     (λ (v)
       (or (reg-pred v) (reference? v))))))

(define-values (rel8off? rel16off? rel32off?)
  ;; FIXME
  (values
   (λ (v) #f)
   (λ (v) #f)
   (λ (v) (or (imm32? v) (symbol? v)))))

(begin-for-syntax
  (struct operand (name predicate keywords) #:transparent))

(define-syntaxes (imm8 imm16 imm32 imm64)
  (apply
   values
   (for/list ([width '(8 16 32 64)]
              [pred (list #'imm8? #'imm16? #'imm32? #'imm64?)])
     (operand
      'immediate
      pred
      `((#:immediate immediate)
        (#:immediate-size ,width))))))

(define-syntaxes (reg8 reg16 reg32 reg64)
  (apply
   values
   (for/list ([width '(8 16 32 64)]
              [pred (list #'reg8? #'reg16? #'reg32? #'reg64?)])
     (operand
      'reg
      pred
      `((#:reg reg)
        (#:operand-size ,width))))))

(define-syntaxes (reg/mem8 reg/mem16 reg/mem32 reg/mem64)
  (apply
   values
   (for/list ([width '(8 16 32 64)]
              [pred (list #'reg/mem8? #'reg/mem16? #'reg/mem32? #'reg/mem64?)])
     (operand
      'r/m
      pred
      `((#:r/m r/m)
        (#:operand-size ,width))))))

(define-syntaxes (rel8off rel16off rel32off)
  (apply
   values
   (for/list ([width '(8 16 32)]
              [pred (list #'rel8off? #'rel16off? #'rel32off?)])
     ;; FIXME
     (operand
      'immediate
      pred
      `((#:immediate immediate))))))
