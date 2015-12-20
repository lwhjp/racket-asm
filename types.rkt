#lang racket

(provide
 (all-defined-out))

(struct instruction (bytes patch!))

(struct label ([address #:mutable]))