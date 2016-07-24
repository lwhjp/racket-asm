#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/stx)
         racket/match
         racket/stxparam
         "operand.rkt"
         "opstr.rkt")

(provide (all-from-out "operand.rkt")
         op
         define-instruction)

(define-syntax-parameter op
  (λ (stx)
    (raise-syntax-error 'op "invalid outside of define-instruction" stx)))

(define-syntax (define-instruction stx)
  (define (syntax->argument arg-stx)
    (syntax-local-value
     arg-stx
     (λ ()
       (raise-syntax-error
        'define-instruction
        "invalid operand specifier"
        stx
        arg-stx))))
  (define (make-clause stx)
    (syntax-case stx ()
      [((arg ...) make-expr)
       (let* ([args (stx-map syntax->argument #'(arg ...))]
              [arg-names (map operand-name args)]
              [arg-predicates (map operand-predicate args)]
              [kws-out (remove-duplicates
                        (sort
                         (append*
                          (map operand-keywords args))
                         keyword<?
                         #:key car))])
         (with-syntax ([(arg-name ...) arg-names]
                       [(arg-pred ...) arg-predicates]
                       [(kw ...) (map car kws-out)]
                       [(kw-val ...) (map cadr kws-out)])
           #'[(list (? arg-pred arg-name) ...)
              (syntax-parameterize
                  ([op (λ (stx)
                         (syntax-case stx ()
                           [(_ str extra-arg (... ...))
                            (compile-opstr (syntax-e #'str) '(kw ...) #'(extra-arg (... ...)))]))])
                (keyword-apply
                 make-expr
                 '(kw ...)
                 (list kw-val ...)
                 '()))]))]))
  (syntax-case stx ()
    [(_ mnemonic clause ...)
     #`(define mnemonic
         (match-lambda*
           #,@(stx-map make-clause #'(clause ...))
           [_ (error 'mnemonic "illegal operand(s)")]))]))
