#!r6rs
;; Utilities to do with numbers that aren't useful enough to go in
;; (ijputils common)
(library (ijputils numbers)
(export proper-fraction)
(import (rnrs base))

(define (proper-fraction n)
  (let ((f (floor n)))
    (values f (- n f))))

)
