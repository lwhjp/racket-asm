#lang racket/base

(provide (all-defined-out))

(require
  (for-syntax racket/base
              syntax/parse)
  data/gvector
  racket/contract/base
  "link.rkt"
  "object.rkt")

(struct assembler
  (out
   symbols
   relocations))

(define current-assembler (make-parameter #f))

(define (make-assembler)
  (assembler
   (open-output-bytes)
   (make-hasheq)
   (make-gvector)))

(define (assembler-position [asm (current-assembler)])
  (ensure-assembler! 'assembler-position asm)
  (file-position (assembler-out asm)))

(define (write-datum v [asm (current-assembler)])
  (ensure-assembler! 'write-datum asm)
  (define out (assembler-out asm))
  (cond
    [(bytes? v) (write-bytes v out)]
    [else (error 'write-datum "invalid datum: ~a" v)]))

(define (write-instruction bstr [asm (current-assembler)])
  (ensure-assembler! 'write-instruction asm)
  (write-bytes bstr (assembler-out asm)))

(define (add-relocation! symbol size offset addend type [asm (current-assembler)])
  (ensure-assembler! 'add-relocation! asm)
  (gvector-add!
   (assembler-relocations asm)
   (ao:reference
    symbol
    (+ (assembler-position asm) offset)
    size
    addend
    type)))

(define (add-symbol! name
                     [value (assembler-position)]
                     [asm (current-assembler)]
                     #:binding [binding 'global])
  (ensure-assembler! 'add-symbol! asm)
  (define symbols (assembler-symbols asm))
  (when (hash-has-key? symbols name)
    (error 'add-symbol! "symbol already defined: ~a" name))
  (hash-set! symbols name (ao:symbol name value binding)))

(define (get-assembled-object [asm (current-assembler)])
  (link-object/local/relative
   (ao:object
    (list->vector (hash-values (assembler-symbols asm)))
    (gvector->vector (assembler-relocations asm))
    (get-output-bytes (assembler-out asm)))))

(define (ensure-assembler! id asm)
  (cond
    [(not asm) (error 'id "not currently assembling")]
    [(not (assembler? asm)) (raise-argument-error 'id "assembler?" asm)]
    [else (void)]))

(define-syntax (assemble stx)
  (define-splicing-syntax-class instruction
    #:attributes (label expr)
    (pattern (~seq #:datum ~! label:id value:expr)
             #:attr expr #'(begin
                             (add-symbol! label #:binding 'local)
                             (datum value)))
    (pattern (~seq #:global ~! label:id)
             #:attr expr #'(add-symbol! label #:binding 'global))
    (pattern (~seq #:label ~! label:id)
             #:attr expr #'(add-symbol! label #:binding 'local))
    (pattern expr:expr
             #:attr label #f))
  (syntax-parse stx
    [(_ body:instruction ...)
     (with-syntax ([(label ...) (filter syntax? (attribute body.label))]
                   [(expr ...) (filter syntax? (attribute body.expr))])
       #'(plain-assemble
          (define-values (label ...) (values 'label ...))
          expr ...))]))

(define-syntax (plain-assemble stx)
  (syntax-case stx ()
    [(_ body ...)
     #'(let ([asm (make-assembler)])
         (parameterize ([current-assembler asm])
           body ...)
         (get-assembled-object asm))]))
