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
        )
(import (rnrs base)
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

)
