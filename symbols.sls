#!r6rs
(library (ijputils symbols)
(export symbol-append
        symbol<?
        symbol>?
        symbol<=?
        symbol>=?
        )
(import (rnrs)
        (only (ijputils functions) on))

(define (symbol-append . args)
  (string->symbol
   (apply string-append
          (map symbol->string args))))

(define symbol<?  (on string<?  symbol->string))
(define symbol<=? (on string<=? symbol->string))
(define symbol>?  (on string>?  symbol->string))
(define symbol>=? (on string>=? symbol->string))

)
