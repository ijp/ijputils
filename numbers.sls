#!r6rs
;; Utilities to do with numbers that aren't useful enough to go in
;; (ijputils common)
(library (ijputils numbers)
(export proper-fraction
        coprime?
        relatively-prime?
        mod-inverse
        extended-euclidean
        sigmoid
        )
(import (rnrs base) (rnrs control))

(define (proper-fraction n)
  (let ((f (floor n)))
    (values f (- n f))))

(define (coprime? x y)
  (= 1 (gcd x y)))

(define relatively-prime? coprime?)

;; modular multiplicative inverse
;; a^-1 where a^-1 * a = 1 (mod m)
;; we could use euler's theorem instead of the extended euclidean
;; algorithm
(define (mod-inverse a m)
  (unless (coprime? a m)
    (assertion-violation 'mod-inverse
                         "mod-inverse doesn't exist if a and m aren't coprime"
                         (list a m)))
  (let-values ([(r x y) (extended-euclidean a m)])
    ;; y gives the multiplicative inverse of m mod a
    ;; x gives the multiplicative inverse of a mod m
    ;;
    ;; we do a mod just in case it's a negative, but we could easily
    ;; just check sign, and subtract
    (mod x m)))

;; returns three values r,x,y for a given a,b such that
;; r = gcd(a,b) = ax + by
;; i.e. Bézout's identity
;; see https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
(define (extended-euclidean a b)
  (define (loop a b x_1 x_2 y_1 y_2)
    (let-values ([(q r) (div-and-mod a b)])
      (if (zero? r)
          (values b x_1 y_1)
          (loop b
                r
                (- x_2 (* q x_1))
                x_1
                (- y_2 (* q y_1))
                y_1))))
  (cond [(zero? a)
         (values b 0 1)]
        [(zero? b)
         (values a 1 0)]
        [else (loop a b 0 1 1 0)]))

(define (sigmoid t)
  (/ 1 (+ 1 (exp (- t)))))
)
