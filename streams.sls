#!r6rs
(library (ijputils streams)
(export naturals
        fibonaccis
        rationals
        positive-rationals
        )
(import (rnrs)
        (srfi :41 streams)
        (only (ijputils common) reciprocal)
        (only (ijputils numbers) proper-fraction))

(define naturals (stream-from 0))

(define fibonaccis
  (stream-cons 0 (stream-cons 1 (stream-map + fibonaccis (stream-cdr fibonaccis)))))

;; See the paper "Enumerating the Rationals" for details of this
;; implementation.
(define (next x)
  (let-values ([(n y) (proper-fraction x)])
    (reciprocal (+ n 1 (- y)))))

(define positive-rationals
  (stream-iterate next 1))

(define rationals
  (let ((next* (lambda (n)
                 (cond ((zero? n) 1)
                       ((positive? n) (- n))
                       (else (next (- n)))))))
    (stream-iterate next* 0)))

)
