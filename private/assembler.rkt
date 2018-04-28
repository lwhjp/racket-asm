#lang racket/base

(provide
 assembler?
 current-assembler
 make-assembler
 assembler-position
 write-datum
 write-instruction
 add-relocation!
 add-symbol!
 assemble)

(require (for-syntax racket/base
                     syntax/parse)
         binutils/base
         binutils/link
         data/gvector
         racket/contract/base)

(struct assembler
  (sections
   [current-section #:mutable]))

(struct section
  (output
   symbols
   relocations))

(define current-assembler (make-parameter #f))

(define (make-assembler)
  (assembler
   (make-hasheq)
   #f))

(define (make-section)
  (section
   (open-output-bytes)
   (make-hasheq)
   (make-gvector)))

(define (assembler-out [asm (current-assembler)])
  (ensure-assembler! 'assembler-out asm)
  (section-output (assembler-current-section asm)))

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
   (section-relocations (assembler-current-section asm))
   (bin:relocation
    (+ (assembler-position asm) offset)
    size
    symbol
    type
    addend)))

(define (add-symbol! name
                     [value (assembler-position)]
                     [asm (current-assembler)]
                     #:binding [binding 'global])
  (ensure-assembler! 'add-symbol! asm)
  (define symbols (section-symbols (assembler-current-section asm)))
  (when (hash-has-key? symbols name)
    (error 'add-symbol! "symbol already defined: ~a" name))
  (hash-set! symbols name (bin:symbol name value #f binding #f)))

(define (set-section! name [asm (current-assembler)])
  (ensure-assembler! 'set-section! asm)
  (set-assembler-current-section!
   asm
   (hash-ref! (assembler-sections asm) name make-section)))

(define (get-assembled-object [asm (current-assembler)])
  (link-object/local/relative
   (bin:object
    (hash-map
     (assembler-sections asm)
     (λ (name section)
       (define executable? (eq? '|.text| name))
       (bin:section
        (string->bytes/latin-1 (symbol->string name))
        #f ; Let someone else figure it out
        (not executable?)
        executable?
        (get-output-bytes (section-output section))
        (hash-values (section-symbols section))
        (gvector->list (section-relocations section))))))))

(define (ensure-assembler! id asm)
  (cond
    [(not asm) (error id "not currently assembling")]
    [(not (assembler? asm)) (raise-argument-error id "assembler?" asm)]
    [else (void)]))

(define-syntax (assemble stx)
  (define-splicing-syntax-class instruction
    #:attributes (label expr)
    (pattern (~seq #:datum ~! label:id value:expr)
             #:attr expr #'(begin
                             (add-symbol! label #:binding 'local)
                             (write-datum value)))
    (pattern (~seq #:global ~! label:id)
             #:attr expr #'(add-symbol! label #:binding 'global))
    (pattern (~seq #:label ~! label:id)
             #:attr expr #'(add-symbol! label #:binding 'local))
    (pattern (~seq #:section ~! section:id)
             #:attr label #f
             #:attr expr #'(set-section! 'section))
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
           (set-section! '|.text|)
           body ...)
         (get-assembled-object asm))]))
