#!r6rs
(library (ijputils streams)
(export naturals
        fibonaccis
        )
(import (rnrs) (srfi :41 streams))

(define naturals (stream-from 0))

(define fibonaccis
  (stream-cons 0 (stream-cons 1 (stream-map + fibonaccis (stream-cdr fibonaccis)))))

)
