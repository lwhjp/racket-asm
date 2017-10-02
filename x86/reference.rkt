#lang racket/base

(require
  "register.rkt")

(provide
 (all-defined-out))

(struct reference (scale index base offset)
  #:guard
  (λ (scale index base offset name)
    (unless (memv scale '(1 2 4 8))
      (error name "invalid scale: ~a" scale))
    (unless (or (not index) (register? index))
      (error name "invalid index: ~a" index))
    (unless (or (not base) (register? base))
      (error name "invalid base: ~a" base))
    (unless (exact-integer? offset)
      (error name "invalid offset: ~a" offset))
    (unless (or (not index)
                (not base)
                (= (register-width index)
                   (register-width base)))
      (error name "register width mismatch"))
    (values scale index base offset))
  #:transparent)

(define (reference-width ref)
  (cond
    [(reference-index ref) => register-width]
    [(reference-base ref) => register-width]
    [else #f]))

(define (ptr v)
  (cond
    [(reference? v) v]
    [(register? v) (reference 1 #f v 0)]
    [(exact-integer? v) (reference 1 #f #f v)]
    [else (error "invalid pointer component:" v)]))

(define (ptr+ v . vs)
  (for/fold ([p (ptr v)])
            ([v (in-list vs)])
    (define x (ptr v))
    (define-values (scale index)
      (cond
        [(reference-index x)
         (unless (not (reference-index p))
           (error "too many register components"))
         (values (reference-scale x) (reference-index x))]
        [(and (reference-base x)
              (reference-base p))
         (unless (not (reference-index p))
           (error "too many register components"))
         ; FIXME! don't put SP, IP in index
         (values 1 (reference-base x))]
        [else
         (values (reference-scale p) (reference-index p))]))
    (reference scale
               index
               (or (reference-base p) (reference-base x))
               (+ (reference-offset p) (reference-offset x)))))

(define (ptr* v1 v2)
  (if (register? v1)
      (reference v2 v1 #f 0)
      (reference v1 v2 #f 0)))

(module+ test
  (require rackunit)
  (check-equal? (ptr ebx) (reference 1 #f ebx 0))
  (check-equal? (ptr+ ebx ecx) (reference 1 ecx ebx 0))
  (check-equal? (ptr+ (ptr* 2 ebx) ecx) (reference 2 ebx ecx 0))
  (check-equal? (ptr+ (ptr* 2 ebx) ecx 4) (reference 2 ebx ecx 4))
  (check-equal? (ptr+ ecx 4) (reference 1 #f ecx 4))
  (check-equal? (ptr+ (ptr* 2 ebx) 4) (reference 2 ebx #f 4))
  (check-equal? (ptr+ 4) (reference 1 #f #f 4)))