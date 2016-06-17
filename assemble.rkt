#lang racket

(require
  data/gvector
  "link.rkt"
  "object.rkt")

(provide
 assemble
 plain-assemble
 add-reference!
 add-symbol!
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

(define-syntax-rule (assemble body ...)
  (collect-labels (body ...)))

(define-syntax collect-labels
  (syntax-rules ()
    [(_ (body ...))
     (collect-labels (body ...) ())]
    [(_ () (done ...))
     (plain-assemble done ...)]
    [(_ (#:global id body ...) (done ...))
     (collect-labels (body ...)
                     (done ...
                      (add-symbol! 'id #:binding 'global)))]
    [(_ (#:label id body ...) (done ...))
     (collect-labels (body ...)
                     (done ...
                      (add-symbol! 'id #:binding 'local)))]
    [(_ (body rest ...) (done ...))
     (collect-labels (rest ...) (done ... body))]))

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

(define (write-instruction ins)
  (let ([asm (current-assembler)])
    (unless asm
      (error "invalid outside of (assemble ...)"))
    (write-bytes
     ins
     (assembler-out asm))))
