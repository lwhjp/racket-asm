#lang racket/base

;;;
;;; This module defines some helper macros to make specifying the instruction set easier.
;;; Although they are prefixed with stx: here, they are exported without and expand (with
;;; some rewriting of arguments) to similarly-named structure constructors.
;;;
;;; We apologize for any confusion caused.
;;;

(require (for-syntax racket/base
                     racket/list
                     syntax/parse)
         racket/match
         racket/provide
         syntax/parse/define
         "opcode-map.rkt")

(provide
 (filtered-out
  (λ (name)
    (and (regexp-match? #rx"^stx:" name)
         (substring name 4)))
  (all-defined-out)))

(struct splice (entries))

(begin-for-syntax
  (struct register-addend (max-width code) #:prefab))

(define-for-syntax (id-downcase id)
  (datum->syntax
   id
   (string->symbol
    (string-downcase
     (symbol->string
      (syntax-e id))))))

(define-syntax-parser stx:opcode-map
  [(_ [opcode:number entry:expr] ...)
   (for ([opcode-stx (attribute opcode)]
         #:unless (byte? (syntax-e opcode-stx)))
     (raise-syntax-error #f "expected: byte?" this-syntax opcode-stx))
   #`(make-opcode-map #,@(foldr list* '() (attribute opcode) (attribute entry)))])

(define (make-opcode-map . args)
  (define opcodes+instructions
    (let next-entry ([args args])
      (match args
        ['() '()]
        [(list-rest opcode entry rest)
         (if (splice? entry)
             (let next-in-splice ([entries (splice-entries entry)]
                                  [opcode opcode])
               (if (null? entries)
                   (next-entry rest)
                   (cons (cons opcode (car entries))
                         (next-in-splice (cdr entries) (add1 opcode)))))
             (cons (cons opcode entry) (next-entry rest)))])))
  (define ordered-instructions
    (let next-instruction ([entries (sort opcodes+instructions < #:key car)]
                           [opcode 0])
      (cond
        [(eqv? 256 opcode) '()]
        [(null? entries) (build-list (- 256 opcode) not)]
        [else (match-let ([(cons (cons next-opcode instruction) rest) entries])
                (cond
                  [(< next-opcode opcode) (error "duplicate opcode:" next-opcode)]
                  [(> next-opcode opcode) (cons #f (next-instruction entries (add1 opcode)))]
                  [else (cons instruction (next-instruction rest (add1 opcode)))]))])))
  (opcode-map (apply vector-immutable ordered-instructions)))

(define-syntax-parser stx:legacy-prefix-map
  [(_ [prefix entry:expr] ...)
   (for ([prefix-stx (attribute prefix)]
         #:unless (or (eqv? #f (syntax-e prefix-stx)) (byte? (syntax-e prefix-stx))))
     (raise-syntax-error #f "expected: byte? or #f" this-syntax prefix-stx))
   #'(legacy-prefix-map (list (cons prefix entry) ...))])

(define-syntax stx:modrm-reg-map (make-rename-transformer #'make-modrm-reg-map))

(define (make-modrm-reg-map . args)
  (define entries
    (let next-arg ([args args])
      (cond
        [(null? args) '()]
        [(splice? (car args))
         (let next-in-splice ([entries (splice-entries (car args))])
           (if (null? entries)
               (next-arg (cdr args))
               (cons (car entries) (next-in-splice (cdr entries)))))]
        [else (cons (car args) (next-arg (cdr args)))])))
  (unless (eqv? 8 (length entries))
    (error "expected exactly 8 entries"))
  (modrm-reg-map (apply vector-immutable entries)))

(define-syntax-parser stx:instruction
  [(_ (mnemonic:id arg ...)
      (~or
       (~optional (~or
                   (~and (~datum #:legacy) (~bind [mode #''legacy]))
                   (~and (~datum #:64-bit) (~bind [mode #''64-bit]))
                   (~seq #:mode ~! mode:expr))
                  #:defaults ([mode #'#f]))
       (~optional (~seq #:operand-size ~! operand-size:expr)
                  #:defaults ([operand-size #'#f]))
       (~optional (~seq #:default-operand-size/64 ~! default-operand-size/64:expr)
                  #:defaults ([default-operand-size/64 #'32])))
      ...)
   (with-syntax ([mnemonic (id-downcase #'mnemonic)]
                 [(operand ...) (map (λ (arg) (arg->operand/stx this-syntax arg))
                                     (attribute arg))])
     #'(instruction
        'mnemonic
        (list operand ...)
        mode
        operand-size
        default-operand-size/64))])

(define-syntax-parser stx:instruction*
  [(_ mnemonic:id [(arg ...) ...+] opt ...)
   #'(splice (list (stx:instruction (mnemonic arg ...) opt ...) ...))]
  [(_ [(~and (~or mnemonic:id
                  (mnemonic:id specific-arg ...))) ...]
      (~optional (default-arg ...))
      opt ...)
   (with-syntax ([((arg ...) ...) (map (λ (args)
                                         (or args
                                             (attribute default-arg)
                                             (error "no default args")))
                                       (attribute specific-arg))])
     #'(splice (list (stx:instruction (mnemonic arg ...) opt ...) ...)))])

(define-syntax-parser stx:instruction*/+r
  [(_ (mnemonic:id arg ...) opt ...)
   (define-values (make-args replacements)
     (let loop ([args (attribute arg)]
                [make-args (λ (rest) rest)])
       (when (null? args)
         (raise-syntax-error #f "argument to replace not found" this-syntax))
       (define arg-str (symbol->string (syntax-e (car args))))
       (if (regexp-match? #rx"^[ber]\\?\\?$" arg-str)
           (values
            (λ (replacement)
              (make-args (cons replacement (cdr args))))
            (case (string-ref arg-str 0)
              [(#\b) (build-list 8 (λ (i) (register-addend 8 i)))]
              [(#\e) (build-list 8 (λ (i) (register-addend 32 i)))]
              [(#\r) (build-list 8 (λ (i) (register-addend 64 i)))]))
           (loop (cdr args) (λ (rest) (make-args (cons (car args) rest)))))))
   (with-syntax ([((arg ...) ...) (map make-args replacements)])
     #'(stx:instruction* mnemonic [(arg ...) ...] opt ...))])

(define-syntax-parser stx:instruction/16+32
  [(_ (mnemonic/16:id arg/16 ...)
      (mnemonic/32:id arg/32 ...)
      opt ...)
   #'(list
      (stx:instruction (mnemonic/16 arg/16 ...) #:operand-size 16 opt ...)
      (stx:instruction (mnemonic/32 arg/32 ...) #:operand-size 32 opt ...))])

(define-syntax-parser stx:instruction/16+32+64
  [(_ (mnemonic/16:id arg/16 ...)
      (mnemonic/32:id arg/32 ...)
      (mnemonic/64:id arg/64 ...)
      opt ...)
   #'(list
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

(define-for-syntax (arg->operand/stx ctx arg-stx)
  (define arg-datum (syntax->datum arg-stx))
  (cond
    [(register-addend? arg-datum)
     #`(operand:addend-register #,(case (register-addend-max-width arg-datum)
                                    [(8) #'(data-type:fixed 8)]
                                    [(32) #'(data-type:variant '(16 32))]
                                    [(64) #'(data-type:variant '(16 32 64))])
                                #,(register-addend-code arg-datum))]
    [(symbol? arg-datum) (arg-str->operand-stx ctx arg-stx (symbol->string arg-datum))]
    [(exact-nonnegative-integer? arg-datum) arg-stx]
    [else (raise-syntax-error #f "invalid operand specifier" ctx arg-stx)]))

(define-for-syntax (arg-str->operand-stx ctx sub-ctx arg)
  (cond
    ; multiple possibilities
    [(regexp-match #rx"/" arg)
     (define parts (regexp-split #rx"/" arg))
     #`(list #,@(map (λ (part) (arg-str->operand-stx ctx sub-ctx part)) parts))]
    ; specific register (by code)
    [(regexp-match? #rx"^[er][A-Z][A-Z]$" arg)
     (with-syntax
         ([type (case (string-ref arg 0)
                  [(#\e) #'(data-type:variant '(16 32))]
                  [(#\r) #'(data-type:variant '(16 32 64))])]
          [code (or (for/first ([name '("AX" "CX" "DX" "BX" "SP" "BP" "SI" "DI")]
                                [code (in-naturals)]
                                #:when (string=? (substring arg 1) name))
                      code)
                    (raise-syntax-error #f (format "invalid register specifier: ~a" arg) ctx sub-ctx))])
       #'(operand:implicit-register type code))]
    ; specific register (by name)
    [(regexp-match? #rx"^[A-Z][A-Z]$" arg)
     (with-syntax ([name (string->symbol (string-downcase arg))])
       #'(operand:implicit-register #f 'name))]
    ; LEA
    [(string=? "M" arg) #'(operand:modrm-memory #f)]
    ; other operands
    [(regexp-match? #rx"^[A-Z][a-z]+$" arg)
     (with-syntax
         ([type (case (substring arg 1)
                  [("b") #'(data-type:fixed 8)]
                  [("c") #'(data-type:variant '(8 16))]
                  [("d") #'(data-type:fixed 32)]
                  [("p") #'(data-type:far-pointer)]
                  [("q") #'(data-type:fixed 64)]
                  [("v") #'(data-type:variant '(16 32 64))]
                  [("w") #'(data-type:fixed 16)]
                  [("y") #'(data-type:variant '(32 64))]
                  [("z") #'(data-type:variant '(16 32))]
                  [else (raise-syntax-error #f (format "unknown data type specifier: ~a" (substring arg 1)) ctx sub-ctx)])])
       (case (string-ref arg 0)
         [(#\A) #'(operand:far-pointer type)]
         [(#\E) #'(operand:r/m type)]
         [(#\G) #'(operand:reg-general type)]
         [(#\I) #'(operand:immediate type)]
         [(#\J) #'(operand:ip-offset type)]
         [(#\M) #'(operand:modrm-memory type)]
         [(#\O) #'(operand:offset type)]
         [(#\R) #'(operand:r/m-general type)]
         [(#\S) #'(operand:reg-segment type)]
         [else (raise-syntax-error #f (format "unknown operand specifier: ~a" (string-ref arg 0)) ctx sub-ctx)]))]
    ; invalid specifier
    [else (raise-syntax-error #f (format "invalid operand specifier: ~a" arg) ctx sub-ctx)]))
