#lang racket/base

(require "object.rkt")

(provide
 link-object/local/relative)

(define (link-object/local/relative obj)
  (let ([symbols
         (for/hasheq ([s (in-vector (ao:object-symbols obj))])
           (values (ao:symbol-name s) s))]
        [text
         (bytes-copy
          (ao:object-text obj))])
    (ao:object
     (ao:object-symbols obj)
     (list->vector
      (let loop ([refs (vector->list (ao:object-references obj))])
        (if (null? refs)
            '()
            (let ([r (car refs)])
              (cond
                [(and (eq? 'relative (ao:reference-type r))
                      (hash-ref symbols (ao:reference-symbol r) #f))
                 =>
                 (λ (s)
                   (integer->integer-bytes
                    (+ (- (ao:symbol-value s)
                          (ao:reference-offset r))
                       (ao:reference-addend r))
                    (ao:reference-size r)
                    #t
                    (system-big-endian?) ; FIXME
                    text
                    (ao:reference-offset r))
                   (loop (cdr refs)))]
                [else (cons r (loop (cdr refs)))])))))
     text)))

(module+ test
  (require rackunit)
  (define obj
    (ao:object
     (vector
      (ao:symbol 'sym1 4 'local))
     (vector
      (ao:reference 'sym1 0 4 -2 'relative)
      (ao:reference 'sym2 4 4 -2 'relative))
     (make-bytes 8 #xFF)))
  (define result
    (link-object/local/relative obj))
  (check-equal? (ao:object-text result)
                (bytes 2 0 0 0 #xFF #xFF #xFF #xFF))
  (check-eqv? (vector-length (ao:object-references result)) 1))
