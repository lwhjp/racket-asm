#lang racket/base

(require (for-syntax racket/base
                     racket/struct
                     (except-in racklog _)
                     "private/operand.rkt"
                     "private/register.rkt")
         racket/splicing
         "private/operand.rkt")

(provide (all-defined-out))

(splicing-let-syntax
    ([define-registers
      (λ (stx)
        (define (get-regs pred) (cdar (%which (rs) (%let (r) (%set-of r (pred r) rs)))))
        (define (make-id reg) (datum->syntax stx (register-name reg)))
        (define (make-def pred ctor)
          (let ([regs (get-regs pred)])
            (with-syntax ([ctor ctor]
                          [(id ...) (map make-id regs)]
                          [((arg ...) ...) (map struct->list regs)])
              #'(define-values (id ...) (values (ctor 'arg ...) ...)))))
        #`(begin
            #,(make-def %general-register #'general-register)
            #,(make-def %segment-register #'segment-register)))])
  (define-registers))
