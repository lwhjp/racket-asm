#lang racket/base

(require racklog)

(provide
 %length
 %boolean)

(define %length
  (%rel (h t n m)
    [('() 0)]
    [((cons h t) n) (%length t m) (%is n (add1 m))]))

(define (%boolean v) (%or (%= v #f) (%= v #t)))
