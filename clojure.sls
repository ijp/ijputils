#!r6rs
;; A bunch of macros stolen from /inspired by clojure
(library (ijputils clojure)
(export if-let
        when-let
        ->
        ->>
        doto)
(import (rnrs)
        (spells match))

;; if-let & when-let are like the similar forms in clojure

(define-syntax if-let
  (syntax-rules ()
    ((if-let (binding predicate) consequent alternative)
     (let ((val predicate))
       (if val
           (match val [binding consequent]) ; consequent
           alternative)))
    ((if-let (binding predicate) consequent)
     (let ((val predicate))
       (if val
           (match val [binding consequent]) ; consequent
           )))))

(define-syntax when-let
  (syntax-rules ()
    ((when-let (binding predicate) body rest ...)
     (let ((val predicate))
       (when val
         ;; body
         ;; rest ...
         (match val
           [binding body rest ...]))))))



;; see http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/-%3e


;; ->
;; macro
;; Usage: (-> x)
;;        (-> x form)
;;        (-> x form & more)
;; Threads the expr through the forms. Inserts x as the
;; second item in the first form, making a list of it if it is not a
;; list already. If there are more forms, inserts the first form as the
;; second item in second form, etc.
;; Added in Clojure version 1.0

(define-syntax ->
  (lambda (stx)
    (define (combine x form)
      (syntax-case form ()
        [(proc args ...)
         #`(proc #,x args ...)]
        [id
         (identifier? #'id)
         #`(id #,x)]
        [else
         (syntax-violation '-> "Invalid form in ->" form)]))
    (syntax-case stx ()
      [(-> x) #'x]
      [(-> x form forms ...)
       (let (;(form-list (syntax->list #'(form forms ...)))
             (form-list #'(form forms ...)))
         (fold-left combine #'x form-list))])))



;; ->>
;; macro
;; Usage: (->> x form)
;;        (->> x form & more)
;; Threads the expr through the forms. Inserts x as the
;; last item in the first form, making a list of it if it is not a
;; list already. If there are more forms, inserts the first form as the
;; last item in second form, etc.
;; Added in Clojure version 1.1


(define-syntax ->>
  (lambda (stx)
    (define (combine x form)
      (syntax-case form ()
        [(proc args ...)
         #`(proc args ... #,x)]
        [id
         (identifier? #'id)
         #`(id #,x)]
        [else
         (syntax-violation '-> "Invalid form in ->" form)]))
    (syntax-case stx ()
      [(->> x) #'x]
      [(->> x form forms ...)
       (let (;(form-list (syntax->list #'(form forms ...)))
             (form-list #'(form forms ...)))
         (fold-left combine #'x form-list))])))

(define-syntax doto
  (syntax-rules ()    
    ((doto this (fun args ...) ...)
     (let ((result this))
       (fun result args ...)
       ...
       result))))

)
