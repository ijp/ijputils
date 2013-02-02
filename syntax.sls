#!r6rs
(library (ijputils syntax)
;; These should probably be exported at levels 0 and 1 as we most
;; likely want to use them in a syntax-case macro
;; TODO: find out a way to write `syntax?'         
(export syntax->list
        syntax->dotted-list
        syntax-e
        gensym
        make-id
        with-syntax*
        )
(import (rnrs base)
        (rnrs syntax-case)
        (only (srfi :1) dotted-list?)
        (ijputils symbols)
        )
(define (map-if predicate mapper list) ;from (familiars lists)
  (map (lambda (x)
         (if (predicate x)
             (mapper x)
             x))
       list))


(define (syntax->list stxobj)
  (define (inner stx)
    (syntax-case stx ()
      [() '()]
      [(x . rest)
       (cons #'x (inner #'rest))]))
  (assert (list? (syntax->datum stxobj)))
  (inner stxobj))

(define (syntax->dotted-list dlist)
  (define (inner stx)
    (syntax-case stx ()
      [(x . rest)
       (cons #'x (inner #'rest))]
      [x #'x]))
  (assert (dotted-list? (syntax->datum dlist)))
  (inner dlist))

(define (syntax-e obj)
  (syntax-case obj ()
    [(first . rest)
     (cons #'first #'rest)]
    [#(value ...)
     (apply vector #'(value ...))]
    [a (syntax->datum #'a)]))

;; Other ideas,
;; with-syntax*
;; like with-syntax but nested
;; syntax-map
;; like (syntax-map f l) => (map f (syntax->list l))
(define (syntax-map proc l)
  (assert (list? (syntax->datum l)))
  (let loop ((stx-list l) (return-list '()))
    (syntax-case stx-list ()
      [() (reverse return-list)]
      [(first . rest)
       (loop #'rest (cons (proc #'first) return-list))])))
;; block
;; allows interleaving of definitions and expressions
;; syntax-parameters


(define (gensym)
  (syntax->datum (car (generate-temporaries (list 'g)))))


;; may come in handy
(define (make-id stx . syms&ids)
      (datum->syntax stx
                     (apply symbol-append
                            (map-if identifier? syntax->datum syms&ids))))

(define-syntax with-syntax*
  (syntax-rules ()
    [(with-syntax* () body rest ...)
     (with-syntax  () body rest ...)]
    [(with-syntax* (one) body rest ...)
     (with-syntax  (one) body rest ...)]
    [(with-syntax* (car . cdr) body rest ...)
     (with-syntax (car)
       (with-syntax* cdr body rest ...))]))

)
