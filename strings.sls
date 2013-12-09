#!r6rs
(library (ijputils strings)
(export string-translate)
(import (rnrs base)
        (only (srfi :13 strings) string-index string-map))

(define (string-translate string from to)
  (define (convert c)
    (cond ((string-index from c) =>
           (lambda (i)
             (string-ref to i)))
          (else c)))
  (string-map convert string))

)
