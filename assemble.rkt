#lang racket

(require
  (for-syntax syntax/parse)
  data/gvector
  "link.rkt"
  "object.rkt")

(provide
 assemble
 plain-assemble
 add-reference!
 add-symbol!
 datum
 write-instruction)

(struct assembler
  (out
   symbols
   references))

(define (assembler-position asm)
  (file-position (assembler-out asm)))

(define current-assembler
  (make-parameter #f))

(define (make-assembler)
  (assembler
   (open-output-bytes)
   (make-gvector)
   (make-gvector)))

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
         (link-object/local/relative
          (ao:object
           (gvector->vector (assembler-symbols asm))
           (gvector->vector (assembler-references asm))
           (get-output-bytes (assembler-out asm)))))]))

(define (add-reference! symbol size offset addend type)
  (gvector-add!
   (assembler-references (current-assembler))
   (ao:reference
    symbol
    (+ (assembler-position (current-assembler)) offset)
    size
    addend
    type)))

(define (add-symbol! name
                     [value (assembler-position (current-assembler))]
                     #:binding [binding 'global])
  (gvector-add!
   (assembler-symbols (current-assembler))
   (ao:symbol
    name
    value
    binding)))

(define (datum v)
  (write-instruction
   (cond
     [(bytes? v) v]
     [else (error "invalid datum")])))

(define (write-instruction ins)
  (let ([asm (current-assembler)])
    (unless asm
      (error "invalid outside of (assemble ...)"))
    (write-bytes
     ins
     (assembler-out asm))))
