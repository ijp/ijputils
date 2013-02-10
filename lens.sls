#!r6rs
;;; Lens toy based on Asymmetric Lenses in Scala by Tony Morris

;;;; Lens laws
;;;
;;; law 1.
;;; (get lens (set lens r f)) == f
;;;
;;; law 2.
;;; (set lens r (get lens r)) == r
;;;
;;; law 3.
;;; (set lens (set lens r f2) f1) == (set lens r f1)
(library (ijputils lens)
(export make-lens
        lens?
        get
        set
        modify
        compose-lens
        id-lens
        product-lens
        car-lens
        cdr-lens
        )
(import (rnrs))

;; maybe make it a codatatype rather than a record?
(define-record-type lens
  (fields
   (immutable get get*)
   (immutable set set*)))

(define (get lens obj)
  ((get* lens) obj))

(define (set lens obj val)
  ((set* lens) obj val))

(define (modify lens func)
  (lambda (obj)
    (set lens obj (func (get lens obj)))))

(define (compose-lens lens2 lens1)
  (get* lens1) (get* lens2) (set* lens1) (set* lens2)
  (make-lens
   (lambda (obj) (get lens2 (get lens1 obj)))
   (lambda (obj val) (set lens1 obj
                      (set lens2 (get lens1 obj) val)))))

(define id-lens
  (make-lens (lambda (x) x) (lambda (x y) x)))

(define (product-lens l1 l2)
  (make-lens
   (lambda (obj)
     (cons (get l1 (car obj))
           (get l2 (cdr obj))))
   (lambda (obj val)
     (cons (set l1 (car obj) (car val))
           (set l2 (cdr obj) (cdr val))))))

(define car-lens
  (make-lens
   car
   (lambda (pair v)
     (cons v (cdr pair)))))

(define cdr-lens
  (make-lens
   cdr
   (lambda (pair new)
     (cons (car pair) new))))

)
