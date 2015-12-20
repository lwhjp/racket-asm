#lang racket

(require
  "types.rkt")

(provide
 (struct-out object)
 assemble
 fix-label!
 make-forward-label
 make-label
 plain-assemble
 write-instruction)

(struct object (code labels))

(struct assembler
  (instructions position emit)
  #:mutable)

(define current-assembler
  (make-parameter #f))

(define (make-assembler)
  (assembler
   '()
   0
   (λ (asm)
     (call-with-output-bytes
      (λ (out)
        (for/fold ([p 0])
                  ([ins (in-list (reverse (assembler-instructions asm)))])
          (let ([patch! (instruction-patch! ins)])
            (when patch!
              (patch! p)))
          (define bs (instruction-bytes ins))
          (write-bytes bs out)
          (+ p (bytes-length bs))))))))

(define-syntax-rule (assemble body ...)
  (collect-labels (body ...)))

(define-syntax collect-labels
  (syntax-rules ()
    [(_ (body ...))
     (collect-labels (body ...) ())]
    [(_ () (done ...))
     (plain-assemble done ...)]
    [(_ (#:label id body ...) (done ...))
     (collect-labels (body ...)
                     ((define id (make-forward-label))
                      done ...
                      (fix-label! id)))]
    [(_ (body rest ...) (done ...))
     (collect-labels (rest ...) (done ... body))]))

(define-syntax (plain-assemble stx)
  (syntax-case stx ()
    [(_ body ...)
     #'(let ([asm (make-assembler)])
         (parameterize ([current-assembler asm])
           body ...)
         ((assembler-emit asm) asm))]))

(define (fix-label! lbl)
  (when (label-address lbl)
    (error "label already fixed"))
  (set-label-address! lbl (assembler-position (current-assembler))))

(define (make-forward-label)
  (label #f))

(define (make-label)
  (label (assembler-position (current-assembler))))

(define (write-instruction ins)
  (let ([asm (current-assembler)])
    (unless asm
      (error "invalid outside of (assemble ...)"))
    (set-assembler-instructions!
     asm
     (cons ins (assembler-instructions asm)))
    (set-assembler-position!
     asm
     (+ (bytes-length (instruction-bytes ins))
        (assembler-position asm)))))
