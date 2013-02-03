#!r6rs
(library (ijputils functions)
(export on)
(import (rnrs base))

(define (on binop f)
  ;; a specialisation of 'fork' matching haskell's on from Data.Function
  (lambda (x y)
    (binop (f x) (f y))))

)
