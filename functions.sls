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
        ;; inspired by racket
        conjoin
        disjoin
        call
        papply
        papplyr
        curryn
        currynr
        eta
        eta*
        )
(import (rnrs base)
        (rnrs lists)
        (only (ijputils common) define-syntax-rule))

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

;;; functions/macros originally from http://docs.racket-lang.org/unstable/Functions.html
(define (conjoin . preds)
  (lambda args
    (for-all (lambda (f) (apply f args)) preds)))

(define (disjoin . preds)
  (lambda args
    (exists (lambda (f) (apply f args)) preds)))

(define (call x . y)
  (apply x y))

(define (papply f . args-l)
  (lambda args-r
    (apply f (append args-l args-r))))

(define (papplyr f . args-r)
  (lambda args-l
    (apply f (append args-l args-r))))

;; racket/function also has curry and curryr, but these seem to
;; require knowledge of how many parameters a function can have
(define (curryn n f . xs)
  (let loop ((n n) (xs xs))
    (if (zero? n)
        (apply f xs)
        (lambda ys
          (loop (- n 1)
                (append xs ys))))))

(define (currynr n f . xs)
  (let loop ((n n) (xs xs))
    (if (zero? n)
        (apply f xs)
        (lambda ys
          (loop (- n 1)
                (append ys xs))))))

(define-syntax-rule (eta exp)
  (lambda args
    (apply exp args)))

(define-syntax-rule (eta* f x ...)
  (lambda (x ...)
    ;; remember f gets evaluated every time
    (f x ...)))

)
