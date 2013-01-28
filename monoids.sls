#!r6rs
(library (ijputils monoids)
(export mappend
        mempty
        mconcat
        make-monoid
        monoid?
        monoid

        dual
        product
        
        ;; monoids
        sum-monoid
        product-monoid
        any-monoid
        all-monoid
        string-monoid
        list-monoid
        max-monoid
        min-monoid
        first-monoid
        last-monoid
        endo-monoid
        )
(import (rnrs))
;; http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html
;; for why monads are interesting :)
;; http://haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Data-Monoid.html for haskell's Data.Monoid module

;; Haskell type class is
;; class Monoid m where
;;   mempty  :: m
;;   mappend :: m -> m -> m
;;   mconcat :: [m] -> m
;;

;; The class of monoids (types with an associative binary operation
;; that has an identity). Instances should satisfy the following laws:
;;
;;     mappend mempty x = x
;;
;;     mappend x mempty = x
;;
;;     mappend x (mappend y z) = mappend (mappend x y) z
;;
;;     mconcat = foldr mappend mempty

(define-record-type monoid
  (protocol
   (lambda (new)
     (case-lambda
       [(unit op)
        (new unit
             op
             (lambda (list)
               (fold-right op unit list)))]
       [(unit op concat)
        (new unit op concat)])))
  (fields
   (immutable unit mempty)
   (immutable operation mappend)
   (immutable concat mconcat)))

(define (dual monoid)
  ;; makes the dual of a given monoid, i.e. mappend has its arguments
  ;; reversed. A commutative monoid is its own dual.
  (make-monoid (mempty monoid)
               (lambda (x y)
                 ((mappend monoid) y x))))

(define (product monoid1 monoid2)
  ;; makes the product of two monoids, i.e. it forms a monoid on pairs
  ;; such that the car of the pairs is mappended according to monoid1,
  ;; and the cdr of the pairs is mappended according to monoid2
  ;;
  ;; perhaps a product-monoid (and probably a dual-monoid) should be
  ;; separate types, so that I can extract the original monoids from
  ;; them again
  (make-monoid (cons (mempty monoid1)
                     (mempty monoid2))
               (lambda (pair1 pair2)
                 (cons
                  ((mappend monoid1) (car pair1) (car pair2))
                  ((mappend monoid2) (cdr pair1) (cdr pair2))))))

;; example monoids
(define sum-monoid (make-monoid 0 +))
(define product-monoid (make-monoid 1 *))
(define any-monoid (make-monoid #f (lambda (x y) (or x y))))
(define all-monoid (make-monoid #t (lambda (x y) (and x y))))
(define string-monoid (make-monoid "" string-append))
(define list-monoid (make-monoid '() append))
(define max-monoid (make-monoid -inf.0 max))
(define min-monoid (make-monoid +inf.0 min))
(define endo-monoid ; monoid on type (a -> a)
  (let ((id (lambda (x) x))
        (compose (lambda (f g)
                   (lambda (x)
                     (f (g x))))))
    (make-monoid id compose)))
;; If I write a "maybe" monad, then the two below should use that
(define first-monoid
  (make-monoid #f
               (lambda (x y)
                 (or x y))))
(define last-monoid
  (make-monoid #f
               (lambda (x y)
                 (or y x))))

)

;; this would be a good use for syntax parameters methinks (can I do
;; this with mutation in pure r6rs?)

; magma semigroup monoid group
; typeclasses in syntax-case
