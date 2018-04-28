#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract)

(provide (all-defined-out))

;;
;; Registers
;;

(struct register
  (name   ; symbol?
   width) ; (or/c 8 16 32 64)
  #:transparent
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "#<register:~a>" (register-name v))))

(struct general-register register
  (code) ; (integer-in 0 15)
  #:transparent)

(struct segment-register register
  ()
  #:transparent)

;;
;; Immediates
;;

(struct immediate
  (width) ; (or/c 8 16 32 64)
  #:transparent)

(struct immediate:constant immediate
  (value) ; exact-integer?
  #:transparent)

(struct immediate:relocation immediate
  (symbol relative?)
  #:transparent)

(define/contract (immediate->bytes imm)
  (-> immediate? bytes?)
  (define size (arithmetic-shift (immediate-width imm) -3))
  (cond
    [(immediate:constant? imm)
     (if (eqv? 1 size)
         (bytes (immediate:constant-value imm))
         (integer->integer-bytes (immediate:constant-value imm)
                                 size
                                 #t
                                 #f))]
    [(immediate:relocation? imm)
     (make-bytes size)]))

(define/contract (make-immediate size v)
  (-> (or/c 8 16 32 64) (or/c exact-integer? symbol?) immediate?)
  (cond
    [(number? v)
     (unless (> size (integer-length v))
       (error 'make-immediate "value out of bounds"))
     (immediate:constant size v)]
    [(symbol? v)
     (immediate:relocation size v)]))

;;
;; Pointers
;;

; TODO: segment (far pointers)

(struct pointer
  (operand-size) ; (or/c #f 8 16 32 48 64 128)
  #:transparent)

(struct pointer:absolute pointer
  (address) ; immediate?
  #:transparent)

(struct pointer:sib pointer
  (base    ; general-register?
   index   ; (or/c #f general-register?)
   scale   ; (or/c 1 2 4 8)
   offset) ; #f / disp8 / disp32
  #:transparent)

(struct pointer:ip-relative pointer
  (offset) ; immediate?
  #:transparent)

(define (make-simple-pointer size v)
  (cond
    [(register? v) (pointer:sib size v #f 1 #f)]
    [(exact-integer? v) (pointer:absolute size (make-immediate 32 v))]
    [else (error "invalid pointer:" v)]))

(define/contract (make-sib-pointer size scale index base offset)
  (-> (or/c 8 16 32 64 #f)
      (or/c 1 2 4 8)
      (or/c general-register? #f)
      general-register?
      (or/c exact-integer? #f)
      pointer:sib?)
  (define offset-immed
    (and offset
         (immediate:constant (if (> 8 (integer-length offset)) 8 32) offset)))
  (pointer:sib size base index scale offset-immed))

(define (make-sib-pointer/2 size foo bar)
  (if (register? bar)
      (make-sib-pointer size 1 foo bar #f)
      (make-sib-pointer size 1 #f foo bar)))

;;
;; Operand helpers
;;

(define-for-syntax (ptr/size size stx)
  (syntax-parse stx
    #:datum-literals (+ *)
    [(_ address-or-base:expr)
     #`(make-simple-pointer #,size address-or-base)]
    [(_ foo:expr + bar:expr)
     #`(make-sib-pointer/2 #,size foo bar)]
    [(_ index:expr + base:expr + offset:expr)
     #`(make-sib-pointer #,size 1 index base offset)]
    [(_ scale:expr * index:expr + base:expr
        (~optional (~seq + offset:expr) #:defaults ([offset #'#f])))
     #`(make-sib-pointer #,size scale index base offset)]))

(define-syntax (ptr stx)
  (syntax-case stx ()
    [(_ . args) (ptr/size #f stx)]))

(define-for-syntax (make-sized-operand size)
  (syntax-parser
    [(_ immed:expr) #`(make-immediate #,size immed)]
    [(_ (~datum ptr) ptr-arg ...+) (ptr/size size #'(ptr ptr-arg ...))]))

(define-syntax byte (make-sized-operand 8))
(define-syntax word (make-sized-operand 16))
(define-syntax dword (make-sized-operand 32))
(define-syntax qword (make-sized-operand 64))
