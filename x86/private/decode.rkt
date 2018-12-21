#lang racket/base

(require racket/list
         racket/match
         racklog
         "instruction.rkt"
         "mode.rkt"
         "operand.rkt"
         "register.rkt")

(provide make-read-instruction)

(define (make-read-instruction op-map)
  (define (%decode-instruction ins op-tail expr tail)
    (%let (mode opcode _modrm _sib _disp _immed ip mnemonic args immed+disp+tail)
      (%and
        (%= ins (instruction mode (_) (_) (_) (_) (_) (_) opcode _modrm _sib _disp _immed))
        (op-map opcode mnemonic (_) mode ip)
        (%maybe-modrm+sib _modrm _sib immed+disp+tail op-tail)
        (ip ins args mode)
        (%let (disp-tail)
          (%and
            (%parse-immed immed+disp+tail _disp disp-tail)
            (%parse-immed disp-tail _immed tail)))
        (%andmap %arg args)
        (%= expr (cons mnemonic args)))))
  (λ ([in (current-input-port)])
    (let/ec return
      (define bs
        (let ([bstr (peek-bytes 15 0 in)])
          (when (eof-object? bstr) (return eof))
          (bytes->list bstr)))
      (define (bad-instruction)
        (return-instruction `(db ,(car bs)) (cdr bs)))
      (define (return-instruction expr tail)
        (define len (- (length bs) (length tail)))
        (port-commit-peeked len (port-progress-evt in) always-evt in)
        (return (list (take bs len) expr)))
      (let*-values
          ([(ins op-tail) (byte-list->instruction/head+tail bs bad-instruction)]
           [(expr tail)
            (match (%which (expr tail) (%decode-instruction ins op-tail expr tail))
              [`((expr . ,expr) (tail . ,tail)) (values (denorm-expr expr) tail)]
              [#f (bad-instruction)])])
        (return-instruction expr tail)))))

(define (denorm-expr expr)
  (cons (car expr) (map denorm-arg (cdr expr))))

(define (denorm-arg arg)
  (match arg
    [(general-register n _ _) n]
    [(segment-register n _ _) n]
    [(immediate:constant _ v) v]
    [(pointer:absolute _ v) `(ptr ,(denorm-arg v))]
    [(pointer:sib _ b i s d)
     (let ([bt (and b (list (denorm-arg b)))]
           [it (cond
                 [(not i) #f]
                 [(zero? s) (list (denorm-arg i))]
                 [else `(,(arithmetic-shift 1 s) * ,(denorm-arg i))])]
           [dt (and d (list (denorm-arg d)))])
       (cons
        'ptr
        (append* (add-between (filter values (list it bt dt)) '((+))))))]
    ; TODO: ip-relative
    [else arg]))

(define %arg
  (%rel (v b i)
    [(v) (%general-register v) !]
    [(v) (%segment-register v) !]
    [((pointer:sib (_) b i (_) (_))) ! (%arg b) (%arg i)]
    [(v)]))

(define %maybe-modrm+sib
  (%rel (_modrm _sib tail m s)
    [(#f #f tail tail)]
    [(_modrm #f tail(cons m tail))
     (%modrm-byte _modrm m)]
    [(_modrm _sib tail (list* m s tail))
     (%modrm-byte _modrm m)
     (%sib-byte _sib m)]))

(define %modrm-byte
  (%rel (mod reg r/m b)
    [((modrm mod reg r/m) b)
     (%is mod (bitwise-bit-field b 6 8))
     (%is reg (bitwise-bit-field b 3 6))
     (%is r/m (bitwise-bit-field b 0 3))]))

(define %sib-byte
  (%rel (scale index base b)
    [((sib scale index base) b)
     (%is scale (bitwise-bit-field b 6 8))
     (%is index (bitwise-bit-field b 3 6))
     (%is base (bitwise-bit-field b 0 3))]))

(define %parse-immed
  (%rel (bs w v tail)
    [(bs #f bs)]
    [(bs (immediate:constant w v) tail)
     (%member w '(8 16 32 64))
     (%is #t (>= (* 8 (length bs)) w))
     (%is v (integer-bytes->integer (list->bytes (take bs (/ w 8))) #t #f))
     (%is tail (drop bs (/ w 8)))]))

(define (byte-list->instruction/head+tail bs fail)
  (define legacy-prefix-codes
    '(#x66 #x67 #x2E #x3E #x26 #x64 #x65 #x36 #xF0 #xF3 #xF2))
  (define mode (current-assembler-bits))
  (let*-values
      ([(legacy-prefixes bs) (splitf-at bs (λ (b) (memv b legacy-prefix-codes)))]
       [(_rex bs) (cond
                    [(null? bs) (fail)]
                    [(and (eqv? 64 mode)
                          (eqv? #x40 (bitwise-and #xF0 (car bs))))
                     (let ([b (car bs)])
                       (values (rex (bitwise-bit-set? b 3)
                                    (bitwise-bit-set? b 2)
                                    (bitwise-bit-set? b 1)
                                    (bitwise-bit-set? b 0))
                               (cdr bs)))]
                    [else (values #f bs)])]
       [(opcode bs) (match bs
                      [(list-rest #x0F #x0F _) (split-at bs 2)]
                      [(list-rest #x0F #x38 _ _) (split-at bs 3)]
                      [(list-rest #x0F #x3A _ _) (split-at bs 3)]
                      [(list-rest #x0F _ _) (split-at bs 2)]
                      [(list-rest _ _) (split-at bs 1)]
                      [_ (fail)])])
    (values
     (instruction
      mode
      (and (memv #x66 legacy-prefixes) #t)
      (and (memv #x67 legacy-prefixes) #t)
      ; FIXME: handle multiple segment overrides
      (cond
        [(memv #x2E legacy-prefixes) 'cs]
        [(memv #x3E legacy-prefixes) 'ds]
        [(memv #x26 legacy-prefixes) 'es]
        [(memv #x64 legacy-prefixes) 'fs]
        [(memv #x65 legacy-prefixes) 'gs]
        [(memv #x36 legacy-prefixes) 'ss]
        [else #f])
      (and (memv #xF0 legacy-prefixes) #t)
      (or (and (memv #xF3 legacy-prefixes) #xF3)
          (and (memv #xF2 legacy-prefixes) #xF2))
      _rex
      opcode
      (_)
      (_)
      (_)
      (_))
     bs)))
