#lang racket

(require "rfib.rkt"
         "ifib.rkt")

(define (fib n)
  (let loop ([a 1]
             [b 1]
             [n (sub1 n)])
    (if (zero? n)
        b
        (loop b (+ a b 1) (sub1 n)))))

#;(module+ main
  (define x 36)
  (printf "fib(~a) = ~a\n" x (fib x))
  (printf "rfib(~a) = ~a\n" x (rfib x))
  (printf "ifib(~a) = ~a\n" x (ifib x)))