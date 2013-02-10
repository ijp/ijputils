#!r6rs
(library (ijputils common)
(export empty
        sub1
        add1
        sum
        square
        generate-token
        define-syntax-rule
        let/cc
        begin0
        thunk
        block
        push!
        pop!
        and=>
        eof=>
        make-counter
        first
        rest
        non-negative?
        non-positive?
        non-zero?
        incr!
        decr!
        )
(import (rnrs base)
        (only (rnrs io ports) eof-object?)
        (only (rnrs lists) fold-left))

(define empty '())

(define (sub1 n) (- n 1))

(define (add1 n) (+ n 1))

(define (sum l) (fold-left + 0 l))

(define (square x) (* x x))

;; There is plenty of ways to do this, including a unique type, but
;; this is simplest.
(define (generate-token)
  (cons #f #f))

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((define-syntax-rule (template-name . template-args) replacement)
     (define-syntax template-name
       (syntax-rules ()
         ((template-name . template-args)
          replacement))))))

(define-syntax-rule (let/cc tag body bodies ...)
  (call/cc (lambda (tag) body bodies ...)))

(define-syntax-rule (begin0 first rest ...)
  (let-values ((vals first))
    rest ...
    (apply values vals)))

(define-syntax-rule (thunk body rest ...)
  (lambda _ body rest ...))

(define-syntax-rule (block body rest ...)
  (let () body rest ...))

(define-syntax-rule (push! val list)
  (set! list (cons val list)))

(define-syntax-rule (pop! list)
  (begin0
    (car list)
    (set! list (cdr list))))

(define (and=> val proc)
  (and val (proc val)))

(define (eof=> x p)
  (if (eof-object? x)
      x
      (p x)))

(define (make-counter n)
  (lambda ()
    (let ((ret n))
      (set! n (+ n 1))
      ret)))

(define first car)
(define rest cdr)

(define (non-negative? n)
  (not (negative? n)))

(define (non-positive? n)
  (not (positive? n)))

(define (non-zero? n)
  (not (zero? n)))

(define-syntax incr!
  (syntax-rules ()
    ((incr! var)
     (set! var (+ var 1)))
    ((incr! var by)
     (set! var (+ var by)))))

(define-syntax decr!
  (syntax-rules ()
    ((decr! var)
     (set! var (- var 1)))
    ((decr! var by)
     (set! var (- var by)))))

)
