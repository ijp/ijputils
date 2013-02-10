#!r6rs
(library (ijputils hashtables)
(export alist->hashtable
        make-equal-hashtable
        )
(import (rnrs))

(define (make-equal-hashtable)
  (make-hashtable equal-hash equal?))

(define alist->hashtable
  (letrec ((a->h (lambda (a h)
                   (for-each (lambda (pair)
                               (hashtable-set! h (car pair) (cdr pair)))
                             a)
                   h)))
    (case-lambda
      ((alist)
       (a->h alist (make-eqv-hashtable)))
      ((alist hash-function equiv)
       (a->h alist (make-hashtable hash-function equiv))))))

)
