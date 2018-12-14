#lang racket/base

(require (for-syntax racket/base
                     racket/match)
         racket/list
         racket/match
         racket/port
         racklog
         "../../base.rkt"
         "instruction.rkt"
         "mode.rkt"
         "operand.rkt")

(provide define-instruction-encoders)

(define-syntax (define-instruction-encoders stx)
  (syntax-case stx ()
    [(_ instruction-set arity-map)
     (let ([arity-map
            (match (identifier-binding #'arity-map)
              [(list-rest src-mod src-id _) ((dynamic-require src-mod src-id))]
              [else (error "bad identifier")])])
       (with-syntax ([(id ...) (map (λ (v) (datum->syntax stx (car v))) arity-map)]
                     [(arities ...) (map cdr arity-map)])
         #'(define-values (id ...)
             (values
              (make-instruction-proc instruction-set 'id 'arities) ...))))]))

(struct encoded-instruction
  (operand-size bytes add-relocations!-proc)
  #:transparent)

(define (encoded-length ins)
  (bytes-length (encoded-instruction-bytes ins)))

(define (make-instruction-proc instruction-set name arities)
  (define (encoder . args-in)
    (define args (map normalize-arg args-in))
    (define results
      (%find-all (ins os)
        (instruction-set (current-assembler-bits) (cons name args) ins os)))
    (define possible-encodings
      (match results
        ['(#f) (error name "illegal arguments")]
        [(list `((ins . ,instructions) (os . ,operand-sizes)) ...)
         (let ([known-sizes (filter (λ (os) (not (eq? '_ os))) operand-sizes)])
           (unless (or (> 2 (length known-sizes)) (apply = known-sizes))
             (error name "ambiguous arguments")))
         (map encode-instruction instructions operand-sizes)]))
    (let ([asm (current-assembler)]
          [enc (argmin encoded-length possible-encodings)])
      (write-instruction (encoded-instruction-bytes enc) asm)
      ((encoded-instruction-add-relocations!-proc enc) asm)))
  (procedure-rename (procedure-reduce-arity encoder arities) name))

(define (normalize-arg v)
  (cond
    [(register? v) v]
    [(immediate? v) v]
    [(pointer? v) v]
    [(symbol? v) (immediate:relocation (_) v (_))]
    [(exact-integer? v) (immediate:constant (_) v)]))

(define (encode-instruction ins operand-size)
  (match-define (instruction mode
                             operand-size-override?
                             address-size-override?
                             segment
                             lock
                             repeat
                             _rex
                             opcode
                             _modrm
                             _sib
                             disp
                             immed)
    ins)
  (define instruction-bytes
    (with-output-to-bytes
     (λ ()
       (when operand-size-override? (write-byte #x66))
       (when address-size-override? (write-byte #x67))
       (when segment
         (write-byte (case segment
                       [(cs) #x2E] [(ds) #x3E] [(es) #x26]
                       [(fs) #x64] [(gs) #x65] [(ss) #x36])))
       (when lock (write-byte #xF0))
       (when repeat (write-byte repeat))
       (when _rex
         (match-let ([(rex w r x b) _rex])
           (write-byte
            (bitwise-ior #x40
                         (if w #b1000 0)
                         (if r #b0100 0)
                         (if x #b0010 0)
                         (if b #b0001 0)))))
       (write-bytes (list->bytes opcode))
       (when _modrm
         (match-let ([(modrm mod reg r/m) _modrm])
           (write-byte
            (bitwise-ior (arithmetic-shift mod 6)
                         (arithmetic-shift reg 3)
                         r/m))))
       (when _sib
         (match-let ([(sib scale index base) _sib])
           (write-byte
            (bitwise-ior (arithmetic-shift scale 6)
                         (arithmetic-shift index 3)
                         base))))
       (when disp (write-bytes (immediate->bytes disp)))
       (when immed (write-bytes (immediate->bytes immed))))))
  (unless (>= 15 (bytes-length instruction-bytes))
    (error "instruction too long"))
  (encoded-instruction
   operand-size
   instruction-bytes
   (λ (asm)
     (define disp-size (if disp (/ (immediate-width disp) 8) 0))
     (define immed-size (if immed (/ (immediate-width immed) 8) 0))
     (when (immediate:relocation? disp)
       (let ([addend (- (+ disp-size immed-size))])
         (add-relocation! (immediate:relocation-symbol disp)
                          disp-size
                          addend
                          addend
                          (if (immediate:relocation-relative? disp) 'relative 'value))))
     (when (immediate:relocation? immed)
       (let ([addend (- immed-size)])
         (add-relocation! (immediate:relocation-symbol immed)
                          immed-size
                          addend
                          addend
                          (if (immediate:relocation-relative? immed) 'relative 'value)))))))
