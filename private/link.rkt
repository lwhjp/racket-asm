#lang racket/base

(provide
 link!
 link-object/local/relative)

(require binutils/base)

(define (link!
         core
         objects
         #:base [base 0]
         #:offset [offset 0]
         #:symbols [symbols '()])
  (define-values (section-offsets size)
    (for*/fold ([offs (hasheq)]
                [start offset])
               ([object (in-list objects)]
                [section (in-list (bin:object-sections object))])
      (values
       (hash-set offs (section start))
       (+ start (bytes-length (bin:section-data section))))))
  (unless (<= size (bytes-length core))
    (error 'link! "overflow: not enough core to hold objects"))
  (define new-globals
    (for*/fold ([new-syms (hasheq)])
               ([obj (in-list objects)]
                [sec (in-list (bin:object-sections obj))]
                [sym (in-list (bin:section-symbols sec))]
                #:when (eq? 'global (bin:symbol-binding sym)))
      (define name (bin:symbol-name sym))
      (when (or (hash-has-key? symbols name)
                (hash-has-key? new-syms name))
        (error 'link! "duplicate symbol:" name))
      (hash-set new-syms name
                (bin:symbol name
                            (+ base
                               (hash-ref section-offsets sec)
                               (bin:symbol-value sym))
                            #f
                            'global
                            #f))))
  (for ([obj (in-list objects)])
    (define local-symbols
      (for*/hasheq ([sec (in-list (bin:object-sections obj))]
                    [sym (in-list (bin:section-symbols sec))]
                    #:when (eq? 'local (bin:symbol-binding sym)))
        (define name (bin:symbol-name sym))
        (values name
                (bin:symbol name
                            (+ base
                               (hash-ref section-offsets sec)
                               (bin:symbol-value sym))
                            #f
                            'local
                            #f))))
    (for ([sec (in-list (bin:object-sections obj))])
      (define start (hash-ref section-offsets sec))
      (bytes-copy! core start (bin:section-data sec))
      (for ([rel (in-list (bin:section-relocations sec))])
        (define name (bin:relocation-symbol rel))
        (define offset (+ start (bin:relocation-offset rel)))
        (define sym
          (or (hash-ref local-symbols name #f)
              (hash-ref new-globals name #f)
              (hash-ref symbols name #f)
              (error 'link! "unresolved symbol:" name)))
        (define value
          (case (bin:relocation-type rel)
            [(address) (bin:symbol-value sym)]
            [(relative) (- (bin:symbol-value sym) (+ base offset))]
            [else (error 'link! "unsupported relocation type:" (bin:relocation-type rel))]))
        (integer->integer-bytes
         (+ value (bin:relocation-addend rel))
         (bin:relocation-size rel)
         #t
         (system-big-endian?) ; FIXME
         core
         offset))))
  (hash-values new-globals))

(define (link-object/local/relative obj)
  (bin:object
   (for/list ([section (in-list (bin:object-sections obj))])
     (define symbols
       (for/hasheq ([s (in-list (bin:section-symbols section))])
         (values (bin:symbol-name s) s)))
     (define text
       (bytes-copy (bin:section-data section)))
     (struct-copy
      bin:section section
      [relocations
       (let loop ([rels (bin:section-relocations section)])
         (if (null? rels)
             '()
             (let ([r (car rels)])
               (cond
                 [(and (eq? 'relative (bin:relocation-type r))
                       (hash-ref symbols (bin:relocation-symbol r) #f))
                  => (λ (s)
                       (integer->integer-bytes
                        (+ (- (bin:symbol-value s)
                              (bin:relocation-offset r))
                           (bin:relocation-addend r))
                        (bin:relocation-size r)
                        #t
                        (system-big-endian?) ; FIXME
                        text
                        (bin:relocation-offset r))
                       (loop (cdr rels)))]
                 [else (cons r (loop (cdr rels)))]))))]
      [data text]))))

(module+ test
  (require rackunit)
  (define obj
    (bin:object
     (list
      (bin:section
       #f
       #f
       #f
       #f
       (make-bytes 8 #xFF)
       (list
        (bin:symbol 'sym1 4 #f 'local #f))
       (list
        (bin:relocation 0 4 'sym1 'relative -2)
        (bin:relocation 4 4 'sym2 'relative -2))))))
  (define result (link-object/local/relative obj))
  (define result-section (car (bin:object-sections result)))
  (check-equal? (bin:section-data result-section)
                (bytes 2 0 0 0 #xFF #xFF #xFF #xFF))
  (check-eqv? (length (bin:section-relocations result-section)) 1))
