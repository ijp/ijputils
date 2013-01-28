#!r6rs
(library (ijputils bytevectors)
(export subbytevector
        bytevector=?*
        bytevector-fold
        bytevector-zip

        bytevector-xor
        bytevector-ior
        bytevector-and
        )
(import (rnrs base)
        (rnrs bytevectors)
        (rnrs arithmetic bitwise)
        (wak foof-loop))

(define (subbytevector bv start end)  
  (let ((len (bytevector-length bv)))
    (assert (<= 0 start end len))
    (let* ((difference (- end start))
           (new-bv (make-bytevector difference)))
      (bytevector-copy! bv start new-bv 0 difference)
      new-bv)))

(define (bytevector=?* bv1 bv2)
  ;; a bytevector compare that the same amount of work regardless of
  ;; if, and where, they differ 
  ;; see http://codahale.com/a-lesson-in-timing-attacks/
  (let ((len1 (bytevector-length bv1))
        (len2 (bytevector-length bv2)))
    (and (= len1 len2)
         (let loop ((result 0) (idx 0))
           (if (= idx len1)
               (zero? result)
               (loop (bitwise-ior result
                                  (bitwise-xor (bytevector-u8-ref bv1 idx)
                                               (bytevector-u8-ref bv2 idx)))
                     (+ 1 idx)))))))

(define (bytevector-fold kons knil bv) ;; u8 version
  (define len (bytevector-length bv))
  (define (loop i knil)
    (if (= i len)
        knil
        (loop (+ i 1)
              (kons (bytevector-u8-ref bv i)
                    knil))))
  (loop 0 knil))

(define (bytevector-zip f b1 b2)
  (define size
    (min (bytevector-length b1)
         (bytevector-length b2)))
  (define results (make-bytevector size))
  (loop ((for i (up-from 0 (to size)))
         (let val (f (bytevector-u8-ref b1 i)
                     (bytevector-u8-ref b2 i))))
        => results
        (bytevector-u8-set! results i val)))

(define (bytevector-xor b1 b2)
  (bytevector-zip bitwise-xor b1 b2))

(define (bytevector-ior b1 b2)
  (bytevector-zip bitwise-ior b1 b2))

(define (bytevector-and b1 b2)
  (bytevector-zip bitwise-and b1 b2))

)
