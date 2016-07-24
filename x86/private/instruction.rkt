#lang racket

(require
  ffi/unsafe
  "../../assemble.rkt"
  "../reference.rkt"
  "../register.rkt")

(provide
 reg/mem?
 r/m-width
 make-instruction)

(define (reg/mem? v)
  (or (register? v)
      (reference? v)))

(define (r/m-width v)
  (cond
    [(register? v) (register-width v)]
    [(not (reference? v)) #f]
    [(reference-base v) => register-width]
    [(reference-index v) => register-width]
    [else #f]))

(define (make-modrm+sib+rex reg r/m override-width?)
  (define (register->code r)
    (if (register? r)
        (bitwise-ior (register-code r)
                     (if (register-high-byte? r) #b100 0))
        r))
  (define reg-code (register->code reg))
  (define-values (mode r/m-code)
    (cond
      [(not r/m)
       (values #f #f)]
      [(register? r/m)
       (values #b11 (register->code r/m))]
      [(eq? 'ip (reference-base r/m))
       (values #b00 #b101)]
      [(zero? (reference-offset r/m))
       (values #b00
               (if (reference-index r/m)
                   #b100
                   (register-code (reference-base r/m))))]
      [(and (reference-base r/m)
            (eqv? 5 (register-code (reference-base r/m))))
       ; we won't bother with [rBP]+disp8 for now
       (values #b10 #b101)]
      [(and (not (reference-index r/m))
            (reference-base r/m))
       (values #b10 (register-code (reference-base r/m)))]
      [else (values (if (reference-base r/m) #b10 #b00)
                    #b100)]))
  (define modrm
    (and mode
         (bitwise-ior
          (arithmetic-shift mode 6)
          (arithmetic-shift (bitwise-and #x7 reg-code) 3)
          (bitwise-and r/m-code #x7))))
  (define sib
    (and (reference? r/m)
         (or (reference-index r/m)
             (not (reference-base r/m)))
         (bitwise-ior
          (arithmetic-shift (sub1 (integer-length (reference-scale r/m))) 6)
          (arithmetic-shift
           (if (reference-index r/m)
               (bitwise-and #x7 (register-code (reference-index r/m)))
               #b100)
           3)
          (if (reference-base r/m)
              (bitwise-and #x7 (register-code (reference-base r/m)))
              #b101))))
  (define rex
    (let ([w override-width?]
          [r (and modrm (bitwise-bit-set? reg-code 3))]
          [x (and (reference? r/m)
                  (reference-index r/m)
                  (bitwise-bit-set? (register-code (reference-index r/m)) 3))]
          [b (cond
               [(reference? r/m) (and (reference-base r/m)
                                      (bitwise-bit-set? (register-code
                                                         (reference-base r/m))
                                                        3))]
               [(register? r/m) (bitwise-bit-set? (register-code r/m) 3)]
               [reg (bitwise-bit-set? reg-code 3)]
               [else #f])])
      (if (or w r x b)
          (bitwise-ior
           #x40
           (if w #b1000 0)
           (if r #b0100 0)
           (if x #b0010 0)
           (if b #b0001 0))
          #f)))
  (values modrm sib rex))

(define (make-instruction opcode
                          #:reg [reg #f]
                          #:r/m [r/m #f]
                          #:immediate [immediate #f]
                          #:address-size [address-size
                                          (or (and (reference? r/m)
                                                   (reference-width r/m))
                                              64)]
                          #:operand-size [operand-size
                                          (or (and (register? reg)
                                                   (register-width reg))
                                              (and (reg/mem? r/m)
                                                   (r/m-width r/m))
                                              #f)]
                          #:offset-size [offset-size (min 32 (or operand-size 32))]
                          #:immediate-size [immediate-size (min 32 (or operand-size 32))]
                          #:default-operand-size [default-operand-size (if (eqv? 8 operand-size) 8 32)])
  (unless (memv address-size '(32 64))
    (error "address size must be 32 or 64"))
  (unless (or (and (not reg)
                   (not r/m))
              (memv operand-size '(8 16 32 64)))
    (error "invalid operand size"))
  (unless (memv offset-size '(#f 8 16 32 64))
    (error "invalid offset size"))
  (unless (memv immediate-size '(#f 8 16 32 64))
    (error "invalid immediate size"))
  (unless (and (or (not (register? reg))
                   (eqv? operand-size (register-width reg)))
               (or (not (register? r/m))
                   (eqv? operand-size (register-width r/m))))
    (error "register width mismatch"))
  (unless (or (not (reference? r/m))
              (eqv? address-size (or (reference-width r/m) address-size)))
    (error "address size mismatch"))
  (unless (or (not (exact-integer? immediate))
              (and (cpointer? immediate)
                   (<= 64 immediate-size))
              (and immediate-size
                   (< (integer-length immediate) immediate-size)))
    (error "immediate too large for size" immediate-size))
  (unless (or (not offset-size)
              (not r/m)
              (not immediate-size)
              (not immediate)
              (<= (+ offset-size immediate-size) 64))
    (error "total offset and immediate size must be <= 64"))
  (define override-operand-size?
    (cond
      [(not operand-size) #f]
      [(eqv? default-operand-size operand-size) #f]
      [(eqv? 16 operand-size) #t]
      [(eqv? 64 operand-size) #f] ; Use REX instead
      [else (error "invalid operand size override:" operand-size)]))
  (define override-address-size?
    (eqv? 32 address-size))
  (define-values (modrm sib rex)
    (make-modrm+sib+rex reg r/m (and (eqv? 64 operand-size) (not (eqv? 64 default-operand-size)))))
  ;; Write it all out
  (define bs
    (with-output-to-bytes
     (λ ()
       (when override-operand-size?
         (write-byte #x66))
       (when override-address-size?
         (write-byte #x67))
       (when rex
         (write-byte rex))
       (if (bytes? opcode)
           (write-bytes opcode)
           (write-byte opcode))
       (when modrm
         (write-byte modrm))
       (when sib
         (write-byte sib))
       (when (and (reference? r/m)
                  (not (zero? (reference-offset r/m))))
         (write-bytes (integer->integer-bytes (reference-offset r/m)
                                              (/ offset-size 8)
                                              #t
                                              #f)))
       (when immediate
         (let ([v (cond
                    [(symbol? immediate) 0]
                    [(cpointer? immediate) (cast immediate _pointer _uint64)]
                    [else immediate])])
           (if (eqv? 8 immediate-size)
               (write-byte v)
               (write-bytes (integer->integer-bytes
                             v
                             (/ immediate-size 8)
                             #t
                             #f))))))))
  (write-instruction bs)
  (when (symbol? immediate)
    (let ([size (/ immediate-size 8)])
      (add-reference!
       immediate
       size
       (- size)
       (- size)
       'relative))))