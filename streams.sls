#!r6rs
(library (ijputils streams)
(export ;; Utilities
        stream-every
        stream-scale
        stream-cons*
        ;; Streams
        naturals
        fibonaccis
        rationals
        positive-rationals
        primes
        primes/trial
        )
(import (rnrs)
        (srfi :41 streams)
        (pfds heaps)
        (only (ijputils common) reciprocal)
        (only (ijputils numbers) proper-fraction))

;;;; Stream Utilities

(define (stream-every p? s)
  (or (stream-null? s)
       (and (p? (stream-car s))
             (stream-every p? (stream-cdr s)))))

(define (stream-scale n xs)
  (stream-map (lambda (x) (* x n)) xs))

(define-syntax stream-cons*
  (syntax-rules ()
    ((stream-cons* x)
     x)
    ((stream-cons* x y z ...)
     (stream-cons x (stream-cons* y z ...)))))

;;;; Specific Streams

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


;; The trial division implementation of a prime stream
;; useful for comparison purposes
(define primes/trial
  (stream-cons 2 (stream-of x (x in (stream-from 3)) (prime? x))))

(define (prime? x)
  (define possible-factors
    (stream-take-while (lambda (p) (<= (* p p) x)) primes/trial))
  (stream-every (lambda (p) (positive? (mod x p))) possible-factors))


;; Prime Sieve with a Wheel
;; See the paper "The Genuine Sieve of Eratosthenes" by Mellissa O'Neill
(define (sieve xs)
  (define (pq:empty)
    (make-heap (lambda (x y) (< (car x) (car y)))))
  (define (pq:min-key pq)
    (car (heap-min pq)))
  (define pq:min-key/value heap-min)
  (define (pq:insert pq k v)
    (heap-insert pq (cons k v)))
  (define (pq:delete-min-and-insert p k v)
    (heap-insert (heap-delete-min p) (cons k v)))
  (define (insert-prime table p xs)
    (pq:insert table (* p p) (stream-scale p xs)))
  (define (sieve* xs table)
    (if (stream-null? xs)
        xs
        (let ((x  (stream-car xs))
              (xs (stream-cdr xs))
              (next-composite (pq:min-key table)))
          
          (define adjust (lambda (table)
                           (define min (pq:min-key/value table))
                           (define n (car min))
                           (define n* (stream-car (cdr min)))
                           (define ns (stream-cdr (cdr min)))
                           (if (<= n x)
                               (adjust (pq:delete-min-and-insert table n* ns))
                               table)))
          (if (<= next-composite x)
              (sieve* xs (adjust table))
              (stream-cons x (sieve* xs (insert-prime table x xs)))))))
  (if (stream-null? xs)
      xs
      (let ((x  (stream-car xs))
            (xs (stream-cdr xs)))
        (stream-cons x (sieve* xs (insert-prime (pq:empty) x xs))))))

;; TODO: don't hardcode the wheel, but compute in a macro
(define wheel2357
  (stream-cons* 2 4 2 4 6 2 6 4 2 4 6 6 2 6 4 2 6 4 6 8 4 2 4 2 4 8 
 6 4 6 2 4 6 2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10 wheel2357))

(define (spin xs n)
  (stream-cons n (spin (stream-cdr xs) (+ n (stream-car xs)))))

(define primes
  (stream-cons* 2 3 5 7 (sieve (spin wheel2357 11))))

)
