#!r6rs
(library (ijputils private codatatypes)
(export registry
        )
(import (rnrs))

(define registry
  (make-hashtable (lambda (x)
                    (equal-hash (syntax->datum x)))
                  free-identifier=?)))
