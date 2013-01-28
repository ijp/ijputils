#!r6rs
(library (ijputils common)
(export empty
        sub1
        add1)
(import (rnrs base))

(define empty '())

(define (sub1 n) (- n 1))

(define (add1 n) (+ n 1))


)
