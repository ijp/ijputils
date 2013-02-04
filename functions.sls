#!r6rs
(library (ijputils functions)
(export compose
        flip
        id
        identity ; alias
        constant
        constantly ; alias
        complement
        negate ; alias
        always
        never
        on
        )
(import (rnrs base))

(define (compose . funs)
  (lambda args
    (let loop ((args args) (funs (reverse funs)))
      (if (null? funs)
          (apply values args)
          (let-values ((vals (apply (car funs) args)))
            (loop vals (cdr funs)))))))

(define (flip fn)
  (lambda args
    (apply fn (reverse args))))

(define (id x) x)
(define identity id)

(define (constant n)
  (lambda args n))

;; someone suggested constantly, and I like it, but I'm still not sure
(define constantly constant)

(define (complement proc)
  (lambda args
    (not (apply proc args))))

(define negate complement)

(define always (constantly #t))
(define never  (constantly #f))

(define (on binop f)
  ;; a specialisation of 'fork' matching haskell's on from Data.Function
  (lambda (x y)
    (binop (f x) (f y))))

)
