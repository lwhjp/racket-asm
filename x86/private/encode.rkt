#lang racket/base

(require (for-syntax racket/base
                     racket/match
                     syntax/parse
                     "opcode-map.rkt")
         racket/contract
         racket/list
         racket/port
         racket/splicing
         "../../base.rkt"
         "mode.rkt"
         "operand.rkt"
         "prefix.rkt")

(provide define-instructions-from-opcode-map)

(define-syntax (define-instructions-from-opcode-map stx)
  (syntax-parse stx
    [(_ require-spec opcode-map-id:id)
     #`(splicing-let-syntax
           ([define-instructions
              (λ (stx)
                (local-require (only-in require-spec opcode-map-id))
                (syntax-case stx ()
                  [(_ ctx)
                   (opcode-map->instruction-definitions #'ctx opcode-map-id)]))])
         (define-instructions #,stx))]))

(begin-for-syntax
  (struct variant (instruction prefix opcode encode-args) #:transparent))

(define-for-syntax (opcode-map->instruction-definitions ctx opcodes)
  ;; Collect mnemonics and variants
  (define mnemonic-map
    (let parse-opcode-map ([opcode-map-in opcodes]
                           [mnemonic-map (hasheq)]
                           [instruction-prefix #f]
                           [opcode-prefix '()])
      (for/fold ([mnemonic-map mnemonic-map])
                ([(entry opcode) (in-indexed (opcode-map-entries opcode-map-in))]
                 #:when entry)
        (define (add-instructions mnemonic-map #:encode-args [encode-args '()] . instructions)
          (for/fold ([mnemonic-map mnemonic-map])
                    ([instruction instructions]
                     #:when instruction)
            (hash-update
             mnemonic-map
             (instruction-mnemonic instruction)
             (λ (variants)
               (cons (variant instruction
                              instruction-prefix
                              (reverse (cons opcode opcode-prefix))
                              encode-args)
                     variants))
             '())))
        (match entry
          [(? instruction?) (add-instructions mnemonic-map entry)]
          [(list (? instruction?) ...) (apply add-instructions mnemonic-map entry)]
          [(? opcode-map?)
           (parse-opcode-map entry
                             mnemonic-map
                             instruction-prefix
                             (cons opcode opcode-prefix))]
          [(? legacy-prefix-map?)
           (for/fold ([mnemonic-map mnemonic-map])
                     ([entry (legacy-prefix-map-entries entry)])
             (parse-opcode-map (cdr entry)
                               mnemonic-map
                               (car entry)
                               (cons opcode opcode-prefix)))]
          [(? modrm-reg-map?)
           (for/fold ([mnemonic-map mnemonic-map])
                     ([(instruction reg) (in-indexed (modrm-reg-map-instructions entry))]
                      #:when instruction)
             (add-instructions mnemonic-map instruction #:encode-args `(#:reg ,reg)))]
          [else (error "invalid entry in opcode map:" entry)]))))
  (define instruction-definitions
    (for/list ([(mnemonic variants) (in-hash mnemonic-map)])
      #`(define #,(datum->syntax ctx mnemonic)
          #,(compile-instruction mnemonic variants))))
  (with-syntax ([(def ...) instruction-definitions])
    #'(begin def ...)))

(define-for-syntax (compile-instruction name variants)
  (define variant-procs-by-arity
    (for/fold ([vmap (hasheqv)])
              ([variant variants])
      (define instruction (variant-instruction variant))
      (define encode-args
        (append
         `(#:default-operand-size/64 ,(instruction-default-operand-size/64 instruction))
         (variant-encode-args variant)))
      (define arity (length (instruction-operands instruction)))
      (define fixed-operand-size
        (or (instruction-operand-size instruction)
            (not (for/or ([operand (instruction-operands instruction)])
                   (cond
                     [(operand? operand) (variable-size-operand? operand)]
                     [(list? operand) (ormap variable-size-operand? operand)]
                     [else #f])))))
      (hash-update
       vmap
       arity
       (λ (vs)
         (if fixed-operand-size
             (cons (compile-variant variant #f encode-args) vs)
             (let ([sizes (cond
                            [(eq? 'legacy (instruction-mode instruction)) '(16 32)]
                            [(eqv? 64 (instruction-default-operand-size/64 instruction)) '(16 64)]
                            [else '(16 32 64)])])
               (append (map (λ (size) (compile-variant variant size encode-args)) sizes) vs))))
       '())))
  (with-syntax ([name name]
                [((arity variant-proc ...) ...) (hash->list variant-procs-by-arity)])
    #'(make-instruction-proc 'name (list (list arity variant-proc ...) ...))))

(define-for-syntax (variable-size-operand? operand)
  ; TODO: should be a struct property?
  (define type (operand-type operand))
  (or (data-type:variant? type)
      (data-type:far-pointer? type)))

(struct encoded-instruction
  (operand-size bytes add-relocations!-proc)
  #:transparent)

(define (encoded-length ins)
  (bytes-length (encoded-instruction-bytes ins)))

(define/contract (make-instruction-proc name variant-procs)
  (-> symbol?
      (listof (cons/c exact-nonnegative-integer?
                      (listof (unconstrained-domain-> encoded-instruction?))))
      (unconstrained-domain-> void?))
  (λ args
    (define possible-encodings
      (let ([arg-count (length args)])
        (define possible-variants
          (cond
            [(assv arg-count variant-procs) => cdr]
            [else '()]))
        (let next-variant ([procs possible-variants]
                           [return values])
          (if (null? procs)
              (return '())
              (next-variant
               (cdr procs)
               (let/ec fail
                 (let ([ins (apply (car procs) (λ () (fail return)) args)])
                   (λ (ins-rest)
                     (return (cons ins ins-rest))))))))))
    (cond
      [(null? possible-encodings)
       (error name "illegal arguments")]
      [(not (andmap (λ (enc)
                      (eqv? (encoded-instruction-operand-size (car possible-encodings))
                            (encoded-instruction-operand-size enc)))
                    (cdr possible-encodings)))
       (error name "ambiguous arguments")]
      [else
       (let ([asm (current-assembler)]
             [enc (argmin encoded-length possible-encodings)])
         (write-instruction (encoded-instruction-bytes enc) asm)
         ((encoded-instruction-add-relocations!-proc enc) asm))])))

(define-for-syntax (compile-variant variant effective-operand-size encode-args)
  (define instruction (variant-instruction variant))
  (define operands (instruction-operands instruction))
  (define arg-ids (generate-temporaries operands))
  (define mode-ok?-stx
    (cond
      [(or (eq? '64-bit (instruction-mode instruction))
           (eqv? 64 effective-operand-size))
       #'(eqv? 64 (current-assembler-bits))]
      [(or (eq? 'legacy (instruction-mode instruction))
           (and (eqv? 32 effective-operand-size)
                (eqv? 64 (instruction-default-operand-size/64 instruction))))
       #'(not (eqv? 64 (current-assembler-bits)))]
      [else #'#t]))
  (with-syntax ([fail #'fail]
                [(orig-arg ...) (generate-temporaries operands)]
                [(wrapped-arg ...) (generate-temporaries operands)])
    (define variant-encode-args
      (for/fold ([args (if effective-operand-size
                           (append `(#:operand-size ,effective-operand-size) encode-args)
                           encode-args)])
                ([operand operands]
                 [id (syntax->list #'(wrapped-arg ...))])
        (append (operand->encode-args operand id) args)))
    (with-syntax ([(wrap-expr ...)
                   (map (λ (operand arg-id)
                          (operand->wrapper-stx operand effective-operand-size arg-id #'fail))
                        operands
                        (syntax->list #'(orig-arg ...)))])
      #`(λ (fail orig-arg ...)
          (unless #,mode-ok?-stx (fail))
          (let ([wrapped-arg wrap-expr] ...)
            (let-values ([(ins rel) (encode-instruction '#,(variant-opcode variant)
                                                        #,@variant-encode-args)])
              (encoded-instruction #,effective-operand-size ins rel)))))))

(define-for-syntax (operand->encode-args operand id)
  (cond
    [(list? operand) (operand->encode-args (car operand) id)]
    ; TODO: operand:far-pointer
    [(operand:addend-register? operand)
     ; We include the register here to allow use of REX.B, but this
     ; will break ModRM - so make sure there's no reg operand.
     `(#:reg #f #:r/m ,id)]
    [(or (operand:reg-general? operand)
         (operand:reg-segment? operand))
     `(#:reg ,id)]
    [(or (operand:r/m-general? operand)
         (operand:r/m? operand)
         (operand:modrm-memory? operand))
     `(#:r/m ,id)]
    [(operand:ip-offset? operand)
     `(#:disp ,#`(pointer:ip-relative-offset #,id))]
    [(operand:offset? operand)
     `(#:disp ,#`(pointer:absolute-address #,id))]
    [(operand:immediate? operand)
     `(#:immed ,id)]
    [else '()]))

(define-for-syntax (operand->wrapper-stx operand effective-operand-size arg-id fail-id)
  (with-syntax ([((pred . wrap) ...) (operand->pred+wrap-stxs operand effective-operand-size arg-id)])
    #`(cond [pred wrap] ... [else (#,fail-id)])))

(define-for-syntax (operand->pred+wrap-stxs operand effective-operand-size arg-id)
  (cond
    [(list? operand) (apply append (map (λ (op) (operand->pred+wrap-stxs op effective-operand-size arg-id)) operand))]
    [(operand? operand) (operand->pred+wrap-stxs/single operand effective-operand-size arg-id)]
    [else (list (cons #`(equal? '#,operand #,arg-id) arg-id))]))

(define-for-syntax (operand->pred+wrap-stxs/single operand effective-operand-size arg-id)
  (define expected-size (operand-expected-size operand effective-operand-size))
  (define (make-size-check size-stx)
    (and expected-size
         #`(eqv? #,expected-size #,size-stx)))
  (define (make-pred+wrap type-ok?-stx size-ok?-stx wrap-stx)
    (cons
     (if size-ok?-stx #`(and #,type-ok?-stx #,size-ok?-stx) type-ok?-stx)
     (or wrap-stx arg-id)))
  (with-syntax ([v arg-id])
    (match operand
      [(operand:addend-register _ code)
       (list
        (make-pred+wrap
         #`(and (general-register? v) (eqv? #,code (bitwise-and #b111 (general-register-code v))))
         (make-size-check #'(register-width v))
         #f))]
      [(operand:implicit-register _ (? number? code))
       (list
        (make-pred+wrap
         #`(and (general-register? v) (eqv? #,code (general-register-code v)))
         (make-size-check #'(register-width v))
         #f))]
      [(operand:implicit-register _ (? symbol? name))
       (list
        (make-pred+wrap
         #`(and (register? v) (eq? '#,name (register-name v)))
         (make-size-check #'(register-width v))
         #f))]
      [(? operand:far-pointer?)
       (list
        (make-pred+wrap
         #'#f ; TODO
         #f ; TODO
         #f))]
      [(or (? operand:reg-general?) (? operand:r/m-general?))
       (list
        (make-pred+wrap
         #'(general-register? v)
         (make-size-check #'(register-width v))
         #f))]
      [(? operand:reg-segment?)
       (list
        (make-pred+wrap
         #'(segment-register? v)
         #f
         #f))]
      [(? operand:r/m?)
       (list
        (make-pred+wrap
         #'(general-register? v)
         (make-size-check #'(register-width v))
         #f)
        (make-pred+wrap
         #'(pointer? v)
         (make-size-check #'(pointer-operand-size v))
         #f))]
      [(? operand:modrm-memory?)
       (list
        (make-pred+wrap
         #'(pointer? v)
         #'(pointer-operand-size v)
         #f))]
      [(? operand:ip-offset?)
       ; TODO: constant
       ; TODO: use smaller offset size when possible
       (list
        (make-pred+wrap
         #'(symbol? v)
         (make-size-check #'32)
         #'(pointer:ip-relative #f (immediate:relocation 32 v #t))))]
      [(? operand:immediate?)
       (unless expected-size (error "BUG: immediate operand but no expected size"))
       (list
        (make-pred+wrap
         #'(immediate? v)
         (make-size-check #'(immediate-width v))
         #f)
        (make-pred+wrap
         #'(exact-integer? v)
         #`(>= #,expected-size (integer-length v))
         #`(immediate:constant #,expected-size v))
        (make-pred+wrap
         #'(symbol? v)
         #f
         #`(immediate:relocation #,expected-size v)))]
      [(? operand:offset?)
       ; TODO: constant
       (list
        (make-pred+wrap
         #'(symbol? v)
         (make-size-check #'32)
         #'(pointer:absolute #f (immediate:relocation 32 v #f))))])))

(define-for-syntax (operand-expected-size operand effective-operand-size)
  (match (operand-type operand)
    [#f #f]
    [(data-type:fixed size) size]
    [(data-type:variant sizes)
     (unless effective-operand-size (error "BUG: variable-sized operand in fixed-size instruction"))
     (apply max (filter (λ (s) (<= s effective-operand-size)) sizes))]
    [(data-type:far-pointer) effective-operand-size]))

(define/contract (encode-instruction #:reg [reg #f]
                                     #:r/m [r/m #f]
                                     #:disp [disp #f]
                                     #:immed [immed #f]
                                     #:operand-size [operand-size #f]
                                     #:default-operand-size/64 [default-operand-size/64 32]
                                     opcode)
  (->* ((and/c (listof byte?) (not/c null?)))
       (#:reg (or/c general-register? (integer-in 0 7) #f)
        #:r/m (or/c general-register? pointer? #f)
        #:disp (or/c immediate? #f)
        #:immed (or/c immediate? #f)
        #:operand-size (or/c 16 32 64 #f)
        #:default-operand-size/64 (or/c 32 64))
       (values bytes? (-> assembler? void?)))
  ; r/m with no reg signifies a register addend
  ; TODO: byte-register addressing etc - § 1.8
  ; TODO: more sanity checks
  ; TODO: VEX, XOP
  (define default-operand-size
    (cond
      [(eqv? 64 (current-assembler-bits)) default-operand-size/64]
      [else (current-assembler-bits)]))
  (define-values (rex modrm sib)
    (make-rex+modrm+sib (and (eqv? 64 operand-size)
                             (not (eqv? default-operand-size operand-size)))
                        reg
                        r/m))
  (define r/m-disp
    (cond
      [(pointer:absolute? r/m) (pointer:absolute-address r/m)]
      [(pointer:sib? r/m) (if (eqv? #b101 (general-register-code (pointer:sib-base r/m)))
                              (immediate:constant 8 0) ; base=rBP requires an offset
                              (pointer:sib-offset r/m))]
      [else #f]))
  (define operand-size-override?
    (and operand-size
         (not (eqv? 64 operand-size))
         (not (eqv? operand-size default-operand-size))))
  (define address-size-override?
    (let ([address-size
           (cond
             [(pointer:sib? r/m) (register-width (pointer:sib-base r/m))]
             [else #f])]
          [default-address-size (current-assembler-bits)])
      (cond
        [(not address-size) #f]
        [(eqv? default-address-size address-size) #f]
        [(and (eqv? 64 default-address-size) (eqv? 32 address-size)) #t]
        [(and (eqv? 32 default-address-size) (eqv? 16 address-size)) #t]
        [(and (eqv? 16 default-address-size) (eqv? 32 address-size)) #t]
        [else (error "invalid address size")])))
  (define legacy-prefixes
    (append
     (current-assembler-prefix)
     (filter
      values
      (list (and operand-size-override? #x66)
            (and address-size-override? #x67)))))
  (unless (>= 4 (length legacy-prefixes))
    (error "too many legacy prefixes"))
  (when (and reg (not r/m))
    (error "can't have reg without r/m"))
  (when (and sib (not modrm))
    (error "can't have SIB without ModRM"))
  (when (and r/m-disp disp)
    (error "multiple displacements"))
  (define instruction-bytes
    (with-output-to-bytes
     (λ ()
       (write-bytes (list->bytes legacy-prefixes))
       (when rex (write-byte rex))
       (write-bytes (list->bytes opcode))
       (when modrm (write-byte modrm))
       (when sib (write-byte sib))
       (let ([disp (or r/m-disp disp)])
         (when disp (write-bytes (immediate->bytes disp))))
       (when immed (write-bytes (immediate->bytes immed))))))
  (unless (>= 15 (bytes-length instruction-bytes))
    (error "instruction too long"))
  (values
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

(define/contract (make-rex+modrm+sib 64-bit-operand? reg r/m)
  (-> boolean?
      (or/c general-register? (integer-in 0 7) #f)
      (or/c general-register? pointer? #f)
      (values (or/c byte? #f)
              (or/c byte? #f)
              (or/c byte? #f)))
  (define rex
    (let ([w 64-bit-operand?]
          [r (and (general-register? reg)
                  (bitwise-bit-set? (general-register-code reg) 3))]
          [x (and (pointer:sib? r/m)
                  (pointer:sib-index r/m)
                  (bitwise-bit-set? (general-register-code (pointer:sib-index r/m)) 3))]
          [b (cond
               [(register? r/m)
                (bitwise-bit-set? (general-register-code r/m) 3)]
               [(pointer:sib? r/m)
                (and (pointer:sib-base r/m)
                     (bitwise-bit-set? (general-register-code (pointer:sib-base r/m)) 3))]
               [else #f])])
      (and
       (or w r x b)
       (bitwise-ior
        #x40
        (if w #b1000 0)
        (if r #b0100 0)
        (if x #b0010 0)
        (if b #b0001 0)))))
  (define-values (modrm sib)
    (if (and reg r/m)
        (let-values
            ([(reg-code) (register-code/modrm reg)]
             [(mod rm-code sib)
              (cond
                [(register? r/m) (values #b11 (register-code/modrm r/m) #f)]
                [(pointer:absolute? r/m)
                 ; In 64-bit mode, mod=00 rm=101 means RIP+disp32
                 (if (eqv? 64 (current-assembler-bits))
                                             (values #b00 #b100 #x25)
                                             (values #b00 #b101 #f))]
                [(pointer:sib? r/m) (make-mod+rm+sib r/m)])])
          (values
           (bitwise-ior (arithmetic-shift mod 6)
                        (arithmetic-shift reg-code 3)
                        rm-code)
           sib))
        (values #f #f)))
  (values rex modrm sib))

(define/contract (make-mod+rm+sib ptr)
  (-> pointer:sib? (values (integer-in 0 3) (integer-in 0 7) (or/c byte? #f)))
  (define index (pointer:sib-index ptr))
  (define base (pointer:sib-base ptr))
  (define offset (pointer:sib-offset ptr))
  (define base-code (register-code/modrm base))
  (define mod
    (cond
      [(and (eqv? #b101 base-code) (not offset)) #b01] ; special case for base=rBP
      [(not offset) #b00]
      [(eqv? 8 (immediate-width offset)) #b01]
      [(eqv? 32 (immediate-width offset)) #b10]
      [else (error "invalid offset width")]))
  (define rm (if (not index) base-code #b100))
  (values
   mod
   rm
   (and (eqv? #b100 rm)
        (let ([scale-code (sub1 (integer-length (pointer:sib-scale ptr)))]
              [index-code (if index (register-code/modrm index) #b100)])
          (bitwise-ior
           (arithmetic-shift scale-code 6)
           (arithmetic-shift index-code 3)
           base-code)))))

(define/contract (register-code/modrm r)
  (-> (or/c general-register? (integer-in 0 7)) (integer-in 0 7))
  (if (general-register? r)
      (bitwise-and #b111 (general-register-code r))
      r))
