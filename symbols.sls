#!r6rs
(library (ijputils symbols)
(export symbol-append)
(import (rnrs))

(define (symbol-append . args)
  (string->symbol
   (apply string-append
          (map symbol->string args))))

)
