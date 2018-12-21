#lang racket/base

(require racklog
         "racklog-lib.rkt"
         "register.rkt"
         "operand.rkt")

(provide (all-defined-out))

(struct instruction
  (mode
   operand-size-override?
   address-size-override?
   segment
   lock
   repeat
   rex
   opcode
   modrm
   sib
   disp
   immed)
  #:transparent)

(struct rex (w r x b) #:transparent)

(struct modrm (mod reg r/m) #:transparent)

(struct sib (scale index base) #:transparent)

; NOTE: order of clauses is important due to cut on encode

;;
;; General instruction properties
;;

(define (%legacy-mode mode)
  (%or (%= mode 16) (%= mode 32)))

(define (%operand-size mode os dos/64) (%operand-size-override mode os dos/64 (_) (_)))

(define (%address-size mode as) (%address-size-override mode as (_)))

(define %register-size
  (%rel (size)
    [((general-register (_) size (_)) size)]))

(define %immediate-size
  (%rel (size)
    [((immediate:constant size (_)) size) !]
    [((immediate:relocation size (_) (_)) size)]))

; This is not a general relation to generate instructions!
(define %valid-instruction
  (%rel (mode oso aso seg lock repeat _rex opcode _modrm _sib _disp _immed)
    [((instruction mode oso aso seg lock repeat _rex opcode _modrm _sib _disp _immed))
     #|
     ; These are enforced elsewhere, so only enable them when debugging to save time
     (%member mode '(16 32 64)) !
     (%boolean oso) !
     (%boolean aso) !
     (%member seg '(#f cs ds es fs gs ss)) !
     (%boolean lock) !
     (%member repeat '(#f #xF3 #xF2)) !
     (%or (%= mode 64) (%= _rex #f)) !
     (%or (%= _rex #f) (%valid-rex _rex)) !
     (%valid-opcode opcode) !
     (%or (%and (%= _modrm #f) (%= _sib #f))
          (%and (%valid-modrm _modrm)
                (%or (%= _sib #f) (%valid-sib _sib)))) !
     |#
     (%or (%= _disp #f) (%valid-immediate _disp)) !
     (%or (%= _immed #f) (%valid-immediate _immed)) !
     ; TODO: check total disp+immed length
     ; TODO: check total instruction length
     ]))

(define %valid-rex
  (%rel (w r x b)
    [((rex w r x b)) (%andmap %boolean (list w r x b))]))

(define %valid-opcode
  (%rel (b)
    [((list b))]
    [((list #x0F b))]
    [((list #x0F #x38 b))]
    [((list #x0F #x3A b))]
    ; TODO: 3DNow, VEX, XOP
    ))

(define %valid-modrm
  (%rel (mod reg r/m)
    [((modrm mod reg r/m))
     (%member mod '(#b00 #b01 #b10 #b11)) !
     (%member reg '(0 1 2 3 4 5 6 7)) !
     (%member r/m '(0 1 2 3 4 5 6 7)) !]))

(define %valid-sib
  (%rel (scale index base)
    [((sib scale index base))
     (%member scale '(#b00 #b01 #b10 #b11)) !
     (%member index '(0 1 2 3 4 5 6 7)) !
     (%member base '(0 1 2 3 4 5 6 7)) !]))

(define %valid-immediate
  (%rel (size value)
    [((immediate:constant size value))
     (%member size '(8 16 32 64)) !
     (%or (%and (%var value) !)
          (%and (%is #t (exact-integer? value))
                (%or (%= 0 value)
                     (%and (%is #t (positive? value))
                           (%is #t (>= size (integer-length value))))
                     (%and (%is #t (negative? value))
                           (%is #t (> size (integer-length value)))))))]
    [((immediate:relocation size value (_)))]))

; Hacky helpers

(define (%unbound->false! v) (%coerce-unbound! v #f))
(define (%unbound->zero! v) (%coerce-unbound! v 0))
(define (%coerce-unbound! v d) (%cut-delimiter (%or (%and (%= v d) !) %true)))

;;
;; Operand properties
;;

(define (%register-addend/byte mode code/8 reg)
  (%register-addend/64 mode 8 code/8 reg))

(define %register-addend/legacy
  (%rel (width code n)
    [(width code (general-register n width code))]))

(define %register-addend/64
  (%rel (mode width code/8 code)
    [(mode width code/8 (general-register (_) width code/8))]
    [(64 width code/8 (general-register (_) width code))
     (%is code (bitwise-ior code/8 #b1000))]))

(define %general-register/code
  (%rel (width code n)
    [(width code (general-register n width code))]))

(define %general-register/name
  (%rel (name width c)
    [(name width (general-register name width c))]))

(define %segment-register/name
  (%rel (name w c)
    [(name (segment-register name w c))]))

(define %reg/mem/size
  (%rel (rm os as)
    [(rm os as) (%or (%register-size rm os)
                     (%pointer/size rm os as))]))

(define %pointer/size
  (%rel (rm os as)
    [(rm os as) (%or (%absolute-ptr/size rm os as)
                     (%sib-ptr/size rm os as))]))

(define %ip-offset/size
  (%rel (os x disp)
    [((immediate:constant os (_)) os)]
    [((immediate:relocation os (_) #t) os)]))

(define %absolute-ptr/size
  (%rel (os as disp)
    [((pointer:absolute os disp) os as) (%immediate-size disp as)]))

(define %sib-ptr/size
  (%rel (os as base index scale disp)
    [((pointer:sib os base index scale disp) os as)
     (%or (%= base #f) (%register-size base as))
     (%or (%= index #f) (%register-size index as))
     (%or (%= disp #f)
          (%let (ds)
            (%and (%immediate-size disp ds)
                  (%member ds '(8 32)))))]))

(define %size/z
  (%rel ()
    [(16 16)]
    [(32 32)]
    [(64 32)]))

;;
;; Instruction generation
;;

(define %operand-size-override
  (%rel (dos/64 r x b)
    ; mode os dos/64 oso rex
    [(16 16 dos/64 #f #f)]
    [(16 32 dos/64 #t #f)]
    [(32 32 dos/64 #f #f)]
    [(32 16 dos/64 #t #f)]
    [(64 16 dos/64 #t #f)]
    [(64 32 32 #f #f)]
    [(64 32 32 #f (rex #f r x b))]
    [(64 64 64 #f (_))]
    [(64 16 dos/64 #t (rex #f r x b))]
    [(64 64 dos/64 (_) (rex #t r x b))]))

(define %address-size-override
  (%rel ()
    ; mode as aso
    [(16 16 #f)]
    [(16 32 #t)]
    [(32 32 #f)]
    [(32 16 #t)]
    [(64 64 #f)]
    [(64 32 #t)]))

;;
;; REX, ModRM, SIB
;;

(define (%rex-r _rex r) (%= _rex (rex (_) r (_) (_))))
(define (%rex-x _rex x) (%= _rex (rex (_) (_) x (_))))
(define (%rex-b _rex b) (%= _rex (rex (_) (_) (_) b)))

(define %extend/rex
  (%rel (reg _rex which code code/8 hi?)
    [(reg code/8 #f which)   (%register-with-rex reg #f)
                             (%= reg (general-register (_) (_) code/8))]
    [(reg code/8 _rex which) (%register-with-rex reg #t)
                             (%= reg (general-register (_) (_) code))
                             (which _rex hi?)
                             (%split/8 code/8 hi? code)]))

(define %split/8
  (%rel (code/8 hi? code)
    [(code/8 hi? code)
     (%nonvar code)
     (%is hi? (bitwise-bit-set? code 3))
     (%is code/8 (bitwise-and code #b111))]
    [(code #f code)
     (%nonvar code)
     (%is #f (bitwise-bit-set? code 3))]
    [(code/8 #t code)
     (%nonvar code/8)
     (%is code (bitwise-ior #b1000 code/8))]))

(define %rex+modrm/reg
  (%rel (_rex reg code/8)
    [(_rex (modrm (_) code/8 (_)) reg)
     (%extend/rex reg code/8 _rex %rex-r)]
    [(#f (modrm (_) code/8 (_)) (segment-register (_) (_) code/8))]
    [((reg (_) #f (_) (_)) (modrm (_) code/8 (_)) (segment-register (_) (_) code/8))]))

(define %rex+modrm+sib+disp/rm
  (%rel (mode _rex _modrm _sib _disp r/m)
    [(mode _rex _modrm #f #f r/m)      (%rm/reg _rex _modrm r/m)]
    [(mode _rex _modrm #f _disp r/m)   (%rm/ptr mode _rex _modrm _disp r/m)]
    [(mode _rex _modrm _sib _disp r/m) (%rm+sib/ptr mode _rex _modrm _sib _disp r/m)]))

(define %rm/reg
  (%rel (_rex reg code/8)
    [(_rex (modrm #b11 (_) code/8) reg)
     (%extend/rex reg code/8 _rex %rex-b)]))

(define %rm/ptr ; no SIB
  (%rel (mode _rex mod _disp base-reg code/8)
    [(mode _rex (modrm #b00 (_) code/8) #f (pointer:sib (_) base-reg #f (_) #f))
     (%extend/rex base-reg code/8 _rex %rex-b)
     (%not (%or (%= code/8 #b100) (%= code/8 #b101)))]
    [(mode _rex (modrm #b00 (_) #b101) _disp (pointer:absolute (_) _disp))
     (%legacy-mode mode)
     (%immediate-size _disp 32)]
    [(64 _rex (modrm #b00 (_) #b101) _disp (pointer:ip-relative (_) _disp))
     (%immediate-size _disp 32)]
    [(mode _rex (modrm mod (_) code/8) _disp (pointer:sib (_) base-reg #f (_) _disp))
     (%or (%and (%= mod #b01) (%immediate-size _disp 8))
          (%and (%= mod #b10) (%immediate-size _disp 32)))
     (%extend/rex base-reg code/8 _rex %rex-b)
     (%not (%= code/8 #b100))]))

; TODO: magic to support [rBP]
; TODO: check SIB.base = DH, BH (docs imply these are not dereferenced)
(define %rm+sib/ptr
  (%rel (mode _rex mod scale-code scale index base _disp base-reg index-reg scale-reg)
    [(mode _rex (modrm #b00 (_) #b100) (sib (_) #b100 #b101) _disp (pointer:absolute (_) _disp))
     (%immediate-size _disp 32)]
    [(mode _rex (modrm mod (_) #b100) (sib scale-code index base) _disp (pointer:sib (_) base-reg index-reg scale _disp))
     (%or (%= index-reg #f) (%sib-scale scale-code scale))
     (%sib-index _rex index index-reg)
     (%sib-base _rex base base-reg mod)
     (%or (%and (%= mod #b00) (%= _disp #f) (%not (%= base #b101)))
          (%and (%= mod #b00) (%immediate-size _disp 32) (%= #f base-reg))
          (%and (%= mod #b01) (%immediate-size _disp 8))
          (%and (%= mod #b10) (%immediate-size _disp 32)))]))

(define %sib-scale
  (%rel ()
    [(#b00 1)]
    [(#b01 2)]
    [(#b10 4)]
    [(#b11 8)]))

(define %sib-index
  (%rel (_rex code reg)
    [(_rex code reg) (%extend/rex reg code _rex %rex-x)
                     (%not (%= code #b100))]
    [(_rex #b100 #f)]))

(define %sib-base
  (%rel (_rex code reg mod)
    [(_rex code reg mod) (%extend/rex reg code _rex %rex-b)
                         (%or (%not (%= code #b101))
                              (%= mod #b01)
                              (%= mod #b10))]
    [(_rex #b101 #f #b00)]))
