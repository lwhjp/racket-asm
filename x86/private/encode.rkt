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
         "operand.rkt"
         "racklog-lib.rkt")

(provide define-instruction-encoders)

(define-syntax (define-instruction-encoders stx)
  (syntax-case stx ()
    [(_ op-map arity-map)
     (let ([arity-map
            (match (identifier-binding #'arity-map)
              [(list-rest src-mod src-id _) ((dynamic-require src-mod src-id))]
              [else (error "bad identifier")])])
       (with-syntax ([(id ...) (map (λ (v) (datum->syntax stx (car v))) arity-map)]
                     [(arities ...) (map cdr arity-map)])
         #'(define-values (id ...)
             (let ([instruction-set (operand-map->encode-relation op-map)])
               (values
                (make-instruction-proc instruction-set 'id 'arities) ...)))))]))

(define (operand-map->encode-relation op-map)
  (define %instruction
    (%rel (mode name args-in args ins)
      [(mode (cons name args-in) ins)
       (%let (arity oso aso seg lock rep _rex opcode _modrm _sib instruction-predicate)
         (%and
           (%length args-in arity)
           (%andmap %normalize-arg args-in args)
           (%= ins (instruction mode oso aso seg lock rep _rex opcode _modrm _sib (_) (_)))
           (%or (%= mode 64) (%and (%legacy-mode mode) (%= _rex #f)))
           (op-map opcode name arity mode instruction-predicate)
           ; Provide some options for arguments with unknown size
           (%andmap %default-arg-size args)
           (%cut-delimiter
            (%and (instruction-predicate ins args mode)
                  ; Don't bother encoding unnecessary prefixes
                  (%andmap %unbound->false! (list oso aso seg lock rep _rex))
                  ; Tidy up REX, ModRM and SIB
                  (%or (%= _rex #f)
                       (%let (w r x b)
                         (%and (%= _rex (rex w r x b))
                               (%andmap %unbound->false! (list w r x b)))))
                  (%or (%= _modrm #f)
                       (%let (mod reg r/m)
                         (%and (%= _modrm (modrm mod reg r/m))
                               (%andmap %unbound->zero! (list mod reg r/m)))))
                  (%or (%= _sib #f)
                       (%let (scale index base)
                         (%and (%= _sib (sib scale index base))
                               (%andmap %unbound->zero! (list scale index base)))))
                  ; Final check (particularly for missized immediates)
                  (%valid-instruction ins)
                  !))))]))
  %instruction)

(struct encoded-instruction
  (bytes add-relocations!-proc)
  #:transparent)

(define (encoded-length ins)
  (bytes-length (encoded-instruction-bytes ins)))

(define (make-instruction-proc instruction-set name arities)
  (define (encoder . args)
    (define results
      (%find-all (ins)
        (instruction-set (current-assembler-bits) (cons name args) ins)))
    (define possible-encodings
      (match results
        ['(#f) (error name "illegal arguments")]
        [(list `((ins . ,instructions)) ...)
         (map encode-instruction instructions)]))
    (let ([asm (current-assembler)]
          [enc (argmin encoded-length possible-encodings)])
      (write-instruction (encoded-instruction-bytes enc) asm)
      ((encoded-instruction-add-relocations!-proc enc) asm)))
  (procedure-rename (procedure-reduce-arity encoder arities) name))

(define %normalize-arg
  (%rel (v w)
    [(v (immediate:relocation w v (_)))
     (%is #t (symbol? v)) !]
    [(v (immediate:constant w v))
     (%is #t (exact-integer? v)) !]
    [(v v)]))

(define %default-arg-size
  (%rel (w d)
    [((pointer:absolute w d)) ! (%default-arg-size d)]
    [((pointer:sib w (_) (_) (_) d)) ! (%or (%= d #f)
                                             (%default-arg-size d 8)
                                             (%default-arg-size d 32))]
    [((pointer:ip-relative w d)) ! (%default-arg-size d)]
    [((immediate:relocation w (_) (_))) !
     ; FIXME: we don't know how big the relocation needs to be when assembling,
     ; so force it to 32 bits to avoid ambiguous operand size.
     (%= w 32)]
    [((immediate:constant w (_))) ! (%member w '(8 16 32 64))]
    [((immediate:constant w (_)) w) !]
    [((_))]))

(define (encode-instruction ins)
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
