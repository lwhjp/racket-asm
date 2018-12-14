#lang racket/base

(require (for-syntax racket/base
                     racket/match)
         racket/list
         racket/match
         racket/provide
         racket/stxparam
         racklog
         syntax/parse/define
         "instruction.rkt"
         "operand.rkt"
         "racklog-lib.rkt")

(provide
 (filtered-out
  (λ (name)
    (and (regexp-match? #rx"^stx:" name)
         (substring name 4)))
  (all-defined-out)))

(define-syntax-parameter current-opcode-map #f)
(define-syntax-parameter current-opcode #f)
(define-syntax-parameter current-modrm-reg #f)
(define-syntax-parameter splice (make-rename-transformer #'splice/opcode))

(begin-for-syntax
 (define-syntax-class byte
   (pattern v #:fail-unless (byte? (syntax-e #'v)) "expected byte")))

(define-for-syntax (id-downcase id)
  (datum->syntax
   id
   (string->symbol
    (string-downcase
     (symbol->string
      (syntax-e id))))))

(define-syntax-parser stx:opcode-map
  [(_) #'%empty-rel]
  [(_ [opcode:byte def:expr] ...+)
   ; Can we (easily) combine these all into a single %rel?
   #'(let ([%ops %empty-rel])
       (syntax-parameterize
           ([current-opcode-map (make-rename-transformer #'%ops)])
         (syntax-parameterize
             ([current-opcode #'opcode])
           def) ...)
       %ops)])

(define-syntax-parser splice/opcode
  [(_ ins ...)
   #`(begin
       #,@(for/list ([ins-stx (attribute ins)]
                     [opcode (in-naturals (syntax-e (syntax-parameter-value #'current-opcode)))])
            (unless (byte? opcode)
              (raise-syntax-error #f "too many instructions" this-syntax))
            #`(syntax-parameterize
                  ([current-opcode #'#,opcode])
                #,ins-stx)))])

(define-syntax-parser splice/modrm
  [(_ (~between ins 8 8) ...)
   #`(begin
       #,@(for/list ([ins-stx (attribute ins)]
                     [reg (in-naturals)]
                     #:unless (eq? #f (syntax-e ins-stx)))
            #`(syntax-parameterize
                  ([current-modrm-reg #'#,reg])
                #,ins-stx)))])

(define-syntax-parser stx:escape-to
  [(_ sub-map:id)
   (with-syntax ([opcode (syntax-parameter-value #'current-opcode)])
     #'(%assert! current-opcode-map (opcode-tail mnemonic arity mode args-predicate instruction-predicate)
         [((cons opcode opcode-tail) mnemonic arity mode args-predicate instruction-predicate)
          (sub-map opcode-tail mnemonic arity mode args-predicate instruction-predicate)]))])

(define-syntax-parser stx:legacy-prefix-map
  [(_ [prefix entry:expr] ...)
   (define prefix-flags
     (for/list ([prefix-stx (attribute prefix)])
       (case (syntax-e prefix-stx)
         [(#f) (list #f #f)]
         [(#x66) (list #t #f)]
         [(#xF2) (list #f #xF2)]
         [(#xF3) (list #f #xF3)]
         [else (raise-syntax-error #f "expected: #f, #x66, #xF2 or #xF3" this-syntax prefix-stx)])))
   (with-syntax ([opcode (syntax-parameter-value #'current-opcode)]
                 [((oso rep) ...) prefix-flags]
                 [(sub-map ...) (generate-temporaries (attribute entry))])
     #'(let ([sub-map entry] ...)
         (%assert! current-opcode-map (opcode-tail mnemonic arity mode ap ip sub-ip)
           [((cons opcode opcode-tail) mnemonic arity mode ap ip)
            (sub-map opcode-tail mnemonic arity mode ap sub-ip)
            (%is ip (λ (ins args mode os as)
                      (%and
                        (%= ins (instruction (_) oso (_) (_) (_) rep (_) (_) (_) (_) (_) (_)))
                        (sub-ip ins args mode os as))))]
           ...)))])

(define-syntax-parser stx:modrm-reg-map
  [(_ (~between ins 8 8) ...) #'(splice/modrm ins ...)]
  [(_ ins*) #'(syntax-parameterize ([splice (make-rename-transformer #'splice/modrm)]) ins*)])

(define-syntax-parser stx:instruction
  [(_ (mnemonic:id arg ...)
      (~or
       (~optional (~or
                   (~and (~datum #:legacy) (~bind [mode-goal #'(%legacy-mode mode)]))
                   (~and (~datum #:64-bit) (~bind [mode-goal #'(%= mode 64)]))))
       (~optional (~seq #:operand-size ~! fixed-operand-size:expr))
       (~optional (~seq #:default-operand-size/64 ~! (~and 64 dos/64))
                  #:defaults ([dos/64 #'32])))
      ...)
   (define modrm-reg-goal
     (let ([reg (syntax-parameter-value #'current-modrm-reg)])
       (and reg #`(%= _modrm (modrm (_) #,reg (_))))))
   (define arg-os-goal
     (cond
       [(ormap variant-arg? (attribute arg)) #'(%operand-size mode operand-size dos/64)]
       [else #f]))
   (define arg-as-goal
     (cond
       [(ormap pointer-arg? (attribute arg)) #'(%address-size mode address-size)]
       [else #f]))
   (define ins-os-goal
     (cond
       [(attribute fixed-operand-size) #`(%operand-size-override mode fixed-operand-size dos/64 operand-size-override? _rex)]
       [arg-os-goal #`(%operand-size-override mode operand-size dos/64 operand-size-override? _rex)]
       [else #f]))
   (define ins-as-goal
     (cond
       [arg-as-goal #'(%address-size-override mode address-size address-size-override?)]
       [else #f]))
   (define arg-ids (generate-temporaries (attribute arg)))
   (define-values (arg-goals ins-goals)
     (for/lists (arg-goals ins-goals)
                ([id arg-ids]
                 [arg-stx (attribute arg)])
       (arg->goals this-syntax
                   arg-stx
                   id)))
   (with-syntax ([opcode (syntax-parameter-value #'current-opcode)]
                 [mnemonic (id-downcase #'mnemonic)]
                 [arity (length arg-ids)]
                 [mode-goal (or (attribute mode-goal) #'%true)]
                 [(arg ...) arg-ids]
                 [(arg-goal ...) (filter values (list* arg-os-goal arg-as-goal arg-goals))]
                 [(ins-goal ...) (filter values (list* modrm-reg-goal ins-os-goal ins-as-goal ins-goals))])
     #'(let ([args-predicate
              (%rel (arg ... mode operand-size address-size)
                [((list arg ...) mode operand-size address-size)
                 arg-goal ...])]
             [instruction-predicate
              (%rel (mode operand-size-override? address-size-override? _segment
                     _lock _repeat _rex _opcode _modrm _sib _disp _immed
                     arg ... operand-size address-size)
                [((instruction mode operand-size-override? address-size-override?
                               _segment _lock _repeat _rex _opcode _modrm _sib _disp _immed)
                  (list arg ...) mode operand-size address-size)
                 ins-goal ...
                 ; HACK: this is easier than passing around lists of touched fields
                 (%andmap %unbound->false! (list _modrm _sib _disp _immed))])])
         (%assert! current-opcode-map (mode)
           [('(opcode) 'mnemonic arity mode args-predicate instruction-predicate) mode-goal])))])

(define-syntax-parser stx:instruction*
  [(_ mnemonic:id [(arg ...) ...+] opt ...)
   #'(splice (stx:instruction (mnemonic arg ...) opt ...) ...)]
  [(_ [(~and (~or mnemonic:id
                  (mnemonic:id specific-arg ...))) ...]
      (~optional (default-arg ...))
      opt ...)
   (with-syntax ([((arg ...) ...) (map (λ (args)
                                         (or args
                                             (attribute default-arg)
                                             (error "no default args")))
                                       (attribute specific-arg))])
     #'(splice (stx:instruction (mnemonic arg ...) opt ...) ...))])

(define-syntax-parser stx:instruction*/+r
  [(_ (mnemonic:id arg ...) opt ...)
   (with-syntax ([(args ...) (build-list 8 (λ (i) #'(arg ...)))])
     #'(stx:instruction* mnemonic [args ...] opt ...))])

(define-syntax-parser stx:instruction/16+32
  [(_ (mnemonic/16:id arg/16 ...)
      (mnemonic/32:id arg/32 ...)
      opt ...)
   #'(begin
      (stx:instruction (mnemonic/16 arg/16 ...) #:operand-size 16 opt ...)
      (stx:instruction (mnemonic/32 arg/32 ...) #:operand-size 32 opt ...))])

(define-syntax-parser stx:instruction/16+32+64
  [(_ (mnemonic/16:id arg/16 ...)
      (mnemonic/32:id arg/32 ...)
      (mnemonic/64:id arg/64 ...)
      opt ...)
   #'(begin
      (stx:instruction (mnemonic/16 arg/16 ...) #:operand-size 16 opt ...)
      (stx:instruction (mnemonic/32 arg/32 ...) #:operand-size 32 opt ...)
      (stx:instruction (mnemonic/64 arg/64 ...) #:operand-size 64 opt ...))])

(define-syntax-parser stx:instruction/legacy+64
  [(_ (mnemonic/legacy:id arg/legacy ...)
      (mnemonic/64-bit:id arg/64-bit ...)
      opt ...)
   #'(list
      (stx:instruction (mnemonic/legacy arg/legacy ...) #:legacy opt ...)
      (stx:instruction (mnemonic/64-bit arg/64-bit ...) #:64-bit opt ...))])

(define-for-syntax (variant-arg? arg-stx)
  ; TODO: there are a few missing here, but it's safe to return #t
  (define arg-datum (syntax-e arg-stx))
  (and
   (symbol? arg-datum)
   (match (symbol->string arg-datum)
     [(or (regexp #rx"^[A-Z][A-Z]$")
          (regexp #rx"^[A-Z][bdijoqw]$")
          "b??")
      #f]
     [_ #t])))

(define-for-syntax (pointer-arg? arg-stx)
  (define arg-datum (syntax-e arg-stx))
  (and
   (symbol? arg-datum)
   (match (symbol->string arg-datum)
     [(or (regexp #rx"^[A-Z][A-Z]$")
          (regexp #rx"^[BCDFGHILNPRSUV][a-z]+$")
          (regexp #rx"^[ber](\\?\\?|[A-Z][A-Z])$"))
      #f]
     [_ #t])))

(define-for-syntax (arg->goals ctx arg-stx id)
  (define arg-datum (syntax-e arg-stx))
  (cond
    [(symbol? arg-datum) (arg->goals/sym ctx arg-stx id)]
    [(exact-nonnegative-integer? arg-datum) (values #`(%= #,id (immediate:constant (_) #,arg-datum)) #f)]
    [else (raise-syntax-error #f "invalid operand specifier" ctx arg-stx)]))

(define-for-syntax (arg->goals/sym ctx arg-stx id)
  (define arg (symbol->string (syntax-e arg-stx)))
  (match arg
    ; multiple possibilities
    [(regexp #rx"/")
     (define-values (arg-preds ins-preds)
       (for/lists (arg-preds ins-preds)
                  ([part (regexp-split #rx"/" arg)])
         (arg->goals/sym ctx (datum->syntax #f (string->symbol part) arg-stx) id)))
     (values
      #`(%or #,@arg-preds)
      #`(%or #,@ins-preds))]
    ; register addend
    [(regexp #rx"^[ber]\\?\\?$")
     (let ([code (bitwise-and #x7 (syntax-e (syntax-parameter-value #'current-opcode)))])
       (values
        (case (string-ref arg 0)
          [(#\b) #`(%register-addend/byte mode #,code #,id)]
          [(#\e) #`(%register-addend/legacy operand-size #,code #,id)]
          [(#\r) #`(%register-addend/64 mode operand-size #,code #,id)])
        (and (not (eqv? #\e (string-ref arg 0)))
             #`(%extend/rex #,id (_) _rex %rex-b))))]
    ; specific register (by code)
    [(regexp #rx"^[er]AX$")
     (values
      #`(%general-register/code operand-size 0 #,id)
      #f)]
    ; specific general register (by name)
    [(regexp #rx"^(AL|CL|DX)$")
     (with-syntax ([name (id-downcase arg-stx)])
       (values
        #`(%general-register/name 'name operand-size #,id)
        #f))]
    ; specific segment register (by name)
    [(regexp #rx"^[CDEFGS]S$")
     (with-syntax ([name (id-downcase arg-stx)])
       (values
        #`(%segment-register/name 'name #,id)
        #f))]
    ; other operands
    [(regexp #rx"^([A-Z][a-z]+|M)$")
     (let
         ([type-goal
           (case (string-ref arg 0)
             [(#\A) #'%fail] ; TODO
             [(#\E) #`(%reg/mem/size #,id size address-size)]
             [(#\G #\R) #`(%= #,id (general-register (_) size (_)))]
             [(#\I) #`(%immediate-size #,id size)]
             [(#\J) #`(%ip-offset/size #,id size)]
             [(#\M) #`(%pointer/size #,id size address-size)]
             [(#\O) #`(%absolute-ptr/size #,id size address-size)]
             [(#\S) #`(%= #,id (segment-register (_) size (_)))]
             [else (raise-syntax-error #f "unknown type specifier" ctx arg-stx)])]
          [size-goal
           (case (substring arg 1)
             [("b") #`(%= size 8)]
             [("p") #'%fail] ; TODO
             [("v") #`(%= size operand-size)]
             [("w") #`(%= size 16)]
             [("z") #`(%size/z operand-size size)]
             [("") #'%true] ; LEA
             [else (raise-syntax-error #f "unknown size specifier" ctx arg-stx)])]
          [ins-goal
           (case (string-ref arg 0)
             [(#\A) #'%fail] ; TODO
             [(#\E #\R #\M) #`(%rex+modrm+sib+disp/rm mode _rex _modrm _sib _disp #,id)]
             [(#\G #\S) #`(%rex+modrm/reg _rex _modrm #,id)]
             [(#\I) #`(%= _immed #,id)]
             [(#\J) #`(%= _disp #,id)]
             [(#\O) #`(%= #,id (pointer:absolute (_) _disp))]
             [else #'%fail])])
       (values
        #`(%let (size) (%and #,type-goal #,size-goal))
        ins-goal))]
    ; invalid specifier
    [else (raise-syntax-error #f "invalid operand specifier" ctx arg-stx)]))

(define-syntax-parser stx:define-instruction-set
  [(_ (id:id arity-map:id) op-map:expr)
   ; TODO: it would be nice to provide the arity map via syntax
   #'(define-values (id arity-map)
       (values
        (operand-map->relation op-map)
        (operand-map->arity-map op-map)))])

(define (operand-map->relation op-map)
  (define %instruction
    (%rel (mode name args ins operand-size)
      [(mode (cons name args) ins) (%instruction (cons name args) ins (_))]
      [(mode (cons name args) ins operand-size)
       (%let (arity oso aso seg lock rep _rex opcode _modrm _sib ap ip as)
         (%and
           (%length args arity)
           (op-map opcode name arity mode ap ip)
           (ap args mode operand-size as)
           (%= ins (instruction mode oso aso seg lock rep _rex opcode _modrm _sib (_) (_)))
           (%or (%= mode 64) (%and (%legacy-mode mode) (%= _rex #f)))
           (ip ins args mode operand-size as)
           ; Don't bother encoding unnecessary prefixes
           (%andmap %unbound->false! (list oso aso seg lock rep _rex))
           ; Tidy up REX, ModRM and SIB
           (%or (%= _rex #f)
                (%let (w r x b) (%and (%= _rex (rex w r x b))
                                      (%andmap %unbound->false! (list w r x b)))))
           (%or (%= _modrm #f)
                (%let (mod reg r/m) (%and (%= _modrm (modrm mod reg r/m))
                                          (%andmap %unbound->zero! (list mod reg r/m)))))
           (%or (%= _sib #f)
                (%let (scale index base) (%and (%= _sib (sib scale index base))
                                               (%andmap %unbound->zero! (list scale index base)))))
           ; Final check (particularly for missized immediates)
           (%valid-instruction ins)))]))
  %instruction)

; HACK: thunkify so that we don't compute this every time the module is instantiated
(define ((operand-map->arity-map op-map))
  (match (%find-all (id arity) (op-map (_) id arity (_) (_) (_)))
    [(list `((id . ,ids) (arity . ,aritys)) ...)
     (define arity-sets
       (for/fold ([hsh (hasheq)])
                 ([id ids]
                  [arity aritys])
         (hash-set hsh id (cons arity (hash-ref hsh id '())))))
     (for/list ([(id arities) (in-hash arity-sets)])
       (cons id (remove-duplicates arities)))]))
