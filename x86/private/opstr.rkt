#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/string)
         "../register.rkt"
         "instruction.rkt")

(provide (for-syntax compile-opstr))

(define-for-syntax (compile-opstr str kws-in extra-args)
  (define in-id-map
    (make-immutable-hasheq
     (map cons kws-in (generate-temporaries kws-in))))
  (define parts (string-split str))
  (define base-opcode (string->number (car parts) 16))
  (define opcode base-opcode)
  (define kws-out
    (for/fold ([kws (hasheq)])
      ([mod (in-list (cdr parts))])
      (case mod
        [("/0" "/1" "/2" "/3" "/4" "/5" "/6" "/7")
         (hash-set kws '#:reg (string->number (substring mod 1)))]
        [("/r")
         ;; TODO: only pass through MODRM args when required
         kws]
        [("cb" "cw" "cd" "cp")
         ;; FIXME: these are not immediates (see operand.rkt)
         kws]
        [("ib" "iw" "id" "iq")
         ;; TODO: check width
         kws]
        [("+rb" "+rw" "+rd" "+rq")
         ;; TODO: check
         (let ([reg-id (hash-ref in-id-map '#:reg)])
           (set!
            opcode
            #`(+ #,base-opcode
                 (bitwise-and #x7 (register-code #,reg-id)))))
         kws]
        ; m64
        ; +i
        [else (error "invalid modifier in opstr:" mod)])))
  (with-syntax ([(kw-pass ...) (flatten (hash->list in-id-map))]
                [(kw-new ...) (flatten (hash->list kws-out))]
                [(extra-arg ...) extra-args])
    #`(λ (kw-pass ...)
        (make-instruction #,opcode kw-pass ... kw-new ... extra-arg ...))))
