#!r6rs
(library (ijputils common)
(export empty
        sub1
        add1
        sum
        square
        )
(import (rnrs base)
        (only (rnrs lists) fold-left))

(define empty '())

(define (sub1 n) (- n 1))

(define (add1 n) (+ n 1))

(define (sum l) (fold-left + 0 l))

(define (square x) (* x x))

)
