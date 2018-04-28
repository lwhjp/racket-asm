#lang racket/base

(require)

(provide (all-defined-out))

;;
;; Data type
;;

(struct data-type () #:transparent)

(struct data-type:fixed data-type (size) #:transparent)
(struct data-type:variant data-type (sizes) #:transparent) ; depends on effective operand size
(struct data-type:far-pointer data-type () #:transparent) ; 32, 48 or 80 bit

;;
;; Operand
;;

(struct operand (type) #:transparent)

; TODO(?) maybe these should be instances of a more general operand struct [location, kind]
(struct operand:addend-register operand (code) #:transparent)
(struct operand:implicit-register operand (code-or-name) #:transparent)
(struct operand:far-pointer operand () #:transparent)
(struct operand:reg-general operand () #:transparent)
(struct operand:reg-segment operand () #:transparent)
(struct operand:r/m-general operand () #:transparent)
(struct operand:r/m operand () #:transparent)
(struct operand:modrm-memory operand () #:transparent)
(struct operand:ip-offset operand () #:transparent)
(struct operand:immediate operand () #:transparent)
(struct operand:offset operand () #:transparent)

;;
;; Opcode map
;;

; TODO: special-casing +r instructions would simplify the generated encoders
(struct instruction
  (mnemonic
   operands
   mode ; (or/c 'legacy '64-bit #f)
   operand-size ; (or/c 16 32 64 #f)
   default-operand-size/64)
  #:transparent)

(struct legacy-prefix-map
  (entries) ; (listof (cons/c (or/c byte? #f) any/c)
  #:transparent)

(struct modrm-reg-map
  (instructions) ; vector of length 8
  #:transparent)

(struct opcode-map
  (entries) ; vector of length 256
  #:transparent)
