#!r6rs
(library (ijputils codatatypes)
(export define-codatatype
        type-merge
        )
(import (except (rnrs) member)
        (srfi :45)
        (only (srfi :1) member count)
        (for (ijputils private codatatypes) expand))

(define-syntax define-codatatype
  (lambda (stx)
    (define (identifier-append stx . args)
      (define id
        (string->symbol
         (fold-left (lambda (old x)
                      (string-append old
                                     (if (symbol? x)
                                         (symbol->string x)
                                         (symbol->string (syntax->datum x)))))
                    ""
                    args)))
      (datum->syntax stx id))
    (syntax-case stx ()
      ((define-codatatype codatatype destructor ...)
       (with-syntax (((%destructor ...) (generate-temporaries #'(destructor ...)))
                     ;; i think this needs to be a gensym for guile
                     ((%codatatype-merge) (generate-temporaries #'(codatatype)))
                     ((make-codatatype) (generate-temporaries #'(codatatype)))
                     (codatatype? (identifier-append #'codatatype
                                                     #'codatatype '?)))
         (hashtable-set! registry #'codatatype #'%codatatype-merge)
         #`(begin
             (define-record-type (codatatype make-codatatype codatatype?)
               (fields (immutable destructor %destructor) ...))
             (define (destructor obj)
               (force (%destructor obj))) ...
             (define-syntax %codatatype-merge
               (lambda (stx)
                 (define *options* #'(destructor ...))
                 (define (valid-options opts)
                   (for-each (lambda (opt)
                               (unless (member opt *options* free-identifier=?)
                                 (syntax-violation 'type-merge
                                                   "Field name is not valid for this codatatype"
                                                   opt)))
                             opts))
                 (define (duplicated? elem list)
                   (define c
                     (count (lambda (x) (free-identifier=? x elem)) list))
                   (not (equal? 1 c)))
                 (define (no-duplicates opts)
                   (for-each (lambda (opt)
                               (when (duplicated? opt opts)
                                 (syntax-violation 'type-merge
                                                   "Field name is duplicated"
                                                   opt)))
                             opts))
                 (define (all-specified? opts)
                   (for-each (lambda (opt)
                               (unless (member opt opts free-identifier=?)
                                 (syntax-violation 'type-merge
                                                   "field not specified"
                                                   opt)))
                             *options*))
                 (syntax-case stx ()
                   ((%codatatype-merge (name arg) (... ...))
                    (begin
                      (valid-options #'(name (... ...)))
                      (no-duplicates #'(name (... ...)))
                      (all-specified? #'(name (... ...)))
                      (let* ((option-alist #'((name . arg) (... ...)))
                             (lookup (lambda (x)
                                       (cdr (assp (lambda (y)
                                                    (free-identifier=? y x))
                                                  option-alist)))))
                        (with-syntax (((ordered-args (... ...))
                                       (map lookup *options*)))
                          #'(make-codatatype (delay ordered-args) (... ...)))))))))))))))

(define-syntax type-merge
  (lambda (stx)
    (syntax-case stx ()
      ((type-merge type (field-name body) ...)
       (let ((func (hashtable-ref registry #'type #f)))
         (unless func
           (syntax-violation 'type-merge
                             "Not a valid codatatype"
                             #'type))
         #`(#,func (field-name body) ...))))))

)
