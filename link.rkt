#lang racket/base

(require "object.rkt")

(provide
 link!
 link-object/local/relative)

(define (link!
         core
         objects
         #:base [base 0]
         #:offset [offset 0]
         #:symbols [symbols '()])
  (define object-offsets
    (let loop ([offs (hasheq)]
               [start offset]
               [objects objects])
      (if (null? objects)
          (begin
            (unless (<= start (bytes-length core))
              (error 'link! "overflow: not enough core to hold objects"))
            offs)
          (let ([size (bytes-length (ao:object-text (car objects)))])
            (loop (hash-set offs (car objects) start)
                  (+ start size)
                  (cdr objects))))))
  (define new-globals
    (for*/fold ([new-syms (hasheq)])
               ([obj (in-list objects)]
                [s (in-vector (ao:object-symbols obj))]
                #:when (eq? 'global (ao:symbol-binding s)))
      (define name (ao:symbol-name s))
      (when (or (hash-has-key? symbols name)
                (hash-has-key? new-syms name))
        (error 'link! "duplicate symbol:" name))
      (hash-set new-syms
                name
                (ao:symbol name
                           (+ base
                              (hash-ref object-offsets obj)
                              (ao:symbol-value s))
                           'global))))
  (for ([obj (in-list objects)])
    (define start (hash-ref object-offsets obj))
    (define local-symbols
      (for/hasheq ([s (in-vector (ao:object-symbols obj))]
                   #:when (eq? 'local (ao:symbol-binding s)))
        (define name (ao:symbol-name s))
        (values name
                (ao:symbol name
                           (+ base
                              start
                              (ao:symbol-value s))
                           'local))))
    (bytes-copy! core
                 start
                 (ao:object-text obj))
    (for ([ref (in-vector (ao:object-references obj))])
      (define name (ao:reference-symbol ref))
      (define offset
        (+ start
           (ao:reference-offset ref)))
      (define sym
        (or (hash-ref local-symbols name #f)
            (hash-ref new-globals name #f)
            (hash-ref symbols name #f)
            (error 'link! "unresolved symbol:" name)))
      (define value
        (case (ao:reference-type ref)
          [(address) (ao:symbol-value sym)]
          [(relative) (- (ao:symbol-value sym)
                         (+ base offset))]
          [else (error 'link! "unsupported reference type:" (ao:reference-type ref))]))
      (integer->integer-bytes
       (+ value
          (ao:reference-addend ref))
       (ao:reference-size ref)
       #t
       (system-big-endian?) ; FIXME
       core
       offset)))
  (hash-values new-globals))

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
