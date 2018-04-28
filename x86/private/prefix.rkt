#lang racket/base

(require racket/contract)

(provide
 current-assembler-prefix
 call-with-prefix
 with-prefix)

(define/contract current-assembler-prefix
  (parameter/c (listof byte?))
  (make-parameter '()))

(define/contract (call-with-prefix prefix thunk)
  (-> (or/c byte? (listof byte?)) (-> any) any)
  (parameterize ([current-assembler-prefix
                  (append (current-assembler-prefix)
                          (if (byte? prefix)
                              (list prefix)
                              prefix))])
    (thunk)))

(define-syntax-rule (with-prefix prefix expr)
  (call-with-prefix prefix (λ () expr)))
