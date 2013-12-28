#!r6rs
(library (ijputils boxes)
(export box
        unbox
        set-box!
        box?
        update-box!
        )
(import (rnrs base)
        (rnrs records syntactic))

(define-record-type (<box> box box?)
  (fields (mutable value unbox set-box!)))

(define (update-box! box f)
  (set-box! box (f (unbox box))))

)
