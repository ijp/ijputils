#!r6rs
(library (ijputils lists)
(export range
        plist->alist
        shift-left
        shift-right
        foldl
        foldr
        foldl1
        foldr1
        ;; aliases for foldl1 and foldr1
        fold-left*
        fold-right*
        pairwise
        uniq ;; A better name for `uniq` may be `squeeze` as used in
        ;; ruby for strings
        uniq-by
        group
        group-by
        remove-from
        insert-at
        chunk
        split-at*
        but-last
        insert-at-each ; meant as a helper for permutations, but may
                       ; be useful in it's own right
        ;;Data.List
        intersperse
        intercalate
        transpose
        subsequences
        permutations
        init ;alias for but-last
        inits
        tails

        ;; N.B. scans use the haskell order for the successor function
        scan-left
        scan-right
        scan-left*
        scan-right*
        scanr
        scanl
        scanr1
        scanl1

        transpose*
        pad
        pad-right

        sum
        product
        maximum
        minimum

        alternates
        merge
        
        plist->alist*
        unzip
        unzip*

        distinct?
        
        map-if

        segregate-by

        insert

        cross-product
        cross-product/apply

        unfold*
        unfold**

        ;; list morphisms
        cata ;; fold-right
        ana  ;; unfold*
        hylo ;; (compose ana cata)
        para ;; special kind of fold-right
             ;; see also pair-fold-right

        refold ;; better name for hylomorphisms

        segments

        snoc

        map-accumulate
        map-accumulate-right
        mapAccumL ;; synonyms for above
        mapAccumR

        partitions
        )
(import (except (rnrs base) map)
        (rnrs control)
        (only (srfi :1) iota alist-cons append-reverse concatenate map make-list zip last drop-right member pair-fold pair-fold-right append-map split-at)
        (except (rnrs lists) member)
        (rnrs hashtables)
        (wak foof-loop)
        (srfi :8)
        (except (ijputils common) sum)
        )

(define (chunk list n)
;  (assert (and (non-negative? n) (list? list)))
  (let loop ((ret empty) (l list))
    (if (null? l)
        (reverse ret)
        (receive (chunk rest) (split-at* l n)
          (loop (cons chunk ret) rest)))))
;;>  (chunk (iota 100) 7)
;; ((0 1 2 3 4 5 6) (7 8 9 10 11 12 13) (14 15 16 17 18 19 20)
;;   (21 22 23 24 25 26 27) (28 29 30 31 32 33 34)
;;   (35 36 37 38 39 40 41) (42 43 44 45 46 47 48)
;;   (49 50 51 52 53 54 55) (56 57 58 59 60 61 62)
;;   (63 64 65 66 67 68 69) (70 71 72 73 74 75 76)
;;   (77 78 79 80 81 82 83) (84 85 86 87 88 89 90)
;;   (91 92 93 94 95 96 97) (98 99))


(define (split-at* l n)
  (assert (and (list? l) (non-negative? n)))
  (let loop ((drop-list l) (n n) (take-list '()))
    (cond ((zero? n)
           (values (reverse take-list) drop-list))
          ((null? drop-list)
           (values (reverse take-list) empty))
          (else
           (loop (cdr drop-list)
                 (sub1 n)
                 (cons (car drop-list) take-list))))))

;; range [start] stop [step] -> ListOf Numbers
;; range is the same as python's range
;; just a toy, to see if I could do it with iota
(define range ; maybe alias this to :
  (case-lambda
    [(stop) (if (positive? stop)
                (iota stop)
                '())]
    [(start stop) (if (> start stop)
                      '()
                      (iota (- stop start) start))]
    [(start stop step)
     (cond ((zero? step)
            (assertion-violation 'range "Step argument must not be zero" step))
           ((positive? step)
            (if (>= start stop)
                '()
                (iota (ceiling (/ (- stop start) step)) start step)))
           ;; negative? step
           ((> start stop)
            (iota (ceiling (/ (- stop start) step)) start step))
           (else '()))]))

;;; plist->alist list -> list
;;
;; takes a plist (i.e. key var key var key var ...) and returns an alist
;; (i.e. ((key var) (key var) ...)
;;
;; originally from my (redis utils)
;; TODO:
;; needs real error checking
(define (plist->alist l)
  (if (odd? (length l))
      (assertion-violation 'plist->alist "Can't make an alist from odd number of arguments")
      (let loop ((old l) (new '()))
        (if (null? old)
            (reverse new)
            (loop (cddr old)
                  (alist-cons (car old) (cadr old) new))))))
;; can be done very simply with unfold e.g.
;; (define (plist->alist l)
;;   (unfold null?
;;           (lambda (x) (list (car x) (cadr x)))
;;           cddr
;;           l))

;; shift-left list item -> list
;;
;; drops the first element from list, and puts item on the end
(define (shift-left old-list new)
  (append (cdr old-list) (list new)))

(define (shift-right old-list new)
  (cons new (but-last old-list)))

(define foldl fold-left)
(define foldr fold-right)
(define (foldl1 kons list)
  (fold-left kons (car list) (cdr list)))
(define (foldr1 kons list)
  (fold-right kons (car list) (cdr list)))
(define fold-left* foldl1)
(define fold-right* foldr1)

;; pairwise (a a -> b) ListOf a -> ListOf b
;; applies a function to the successive pairs of a list, accumulating
;; a new list
;; i.e. (pairwise cons '(a b c d)) ->
;; ((a . b) (b . c) (c . d))
(define (pairwise func list)
  (if (null? (cdr list))
      (error 'pairwise "List must contain at least two elements")
      (loop ((with first (car list) second)
             (for second (in-list (cdr list)))
             (for answer (listing (func first second))))
            => answer)))

(define (uniq-by eqv? ordered-factor-list)
  (if (null? ordered-factor-list)
      '()
      (loop ((with first (car ordered-factor-list) second)
             (for second (in-list (cdr ordered-factor-list)))
             (with groups '() (if (eqv? first second)
                                  groups
                                  (cons first groups))))
            => (reverse (cons first groups))
            ;; => (cond ((null? groups)
            ;;           (list first))
            ;;          ((eqv? first (car groups))
            ;;           (reverse groups))
            ;;          (else
            ;;           (reverse (cons first groups))))
            )))
;;(uniq-by eqv? (string->list "mississippi"))
;;(uniq-by eqv? (string->list "m"))

(define (uniq list)
  (uniq-by eqv? list))
;(uniq (string->list "Mississippi"))

(define (group-by eqv? list)
  (define (rev-cons x y)
    ;; by reversing the inner-groups, you can use an arbitrary
    ;; procedure, and still get a list of lists that can be
    ;; concatenated to produce one equal to the original
    (reverse (cons x y)))
  (if (null? list)
      '()
      (loop continue ((with first (car list) second)
                      (for second (in-list (cdr list)))
                      (with inner-group '())
                      (with groups '()))
            => (reverse (cons (rev-cons first inner-group) groups))
            (if (eqv? first second)
                (continue (=> inner-group (cons first inner-group)))
                (continue (=> inner-group '())
                          (=> groups (cons (rev-cons first inner-group)
                                           groups)))))))

(define (group list) (group-by eqv? list))
;(group-by eqv? (string->list "mississippi"))
;((#\m) (#\i) (#\s #\s) (#\i) (#\s #\s) (#\i) (#\p #\p) (#\i))

(define (remove-from list n1)
  (let loop ((l list) (n n1) (l2 '()))
    (if (zero? n)
        (append-reverse l2 (cdr l))
        (if (null? l)
            (assertion-violation 'remove-from
                                 "Can't remove item from list at index. List isn't long enough." n1)
            (loop (cdr l) (- n 1) (cons (car l) l2))))))

(define (insert-at list n1 obj)
  (let loop ((l list) (n n1) (l2 '()))
    (if (zero? n)
        (append-reverse l2 (cons obj l))
        (if (null? l)
            (assertion-violation 'remove-from
                                 "Can't insert item into list at index. List isn't long enough." n1)
            (loop (cdr l) (- n 1) (cons (car l) l2))))))


;; (define (but-last lst)
;;   (if (null? lst)
;;       (assertion-violation 'but-last "List must be non-empty")
;;       ;; better error message would be "Cannot take all-but-last
;;       ;; elements of the empty list" ?
;;       (loop ((for item pair (in-list lst))
;;              (until (null? (cdr pair)))
;;              (for ans (listing item)))
;;             => ans)))
(define (but-last lst)
  (drop-right lst 1))
;; > (but-last '(a b c d e f))
;; (a b c d e)
;; > (but-last '(a b))
;; (a)
;; > (but-last '())
;; Unhandled exception
;;  Condition components:
;;    1. &assertion
;;    2. &who: but-last
;;    3. &message: "Cannot take all-but-last elements of the empty list"
;;    4. &irritants: ()


;;; Functions from Haskell's Data.List

;; intersperse :: a -> [a] -> [a]
;; The intersperse function takes an element and a list and
;; `intersperses' that element between the elements of the list. For
;; example,
;;  intersperse ',' "abcde" == "a,b,c,d,e"
(define (intersperse interspersee lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
    (cons (car lst)
          (cons interspersee
                (intersperse interspersee (cdr lst))))))
;; > (intersperse 8 '())
;; ()
;; > (intersperse 8 '(9))
;; (9)
;; > (intersperse 8 '(9 1 4 6))
;; (9 8 1 8 4 8 6)


;; intercalate :: [a] -> [[a]] -> [a]Source
;; intercalate xs xss is equivalent to (concat (intersperse xs
;; xss)). It inserts the list xs in between the lists in xss and
;; concatenates the result.
(define (intercalate intercalatee lists)
  (concatenate (intersperse intercalatee lists)))
;; > (intercalate '(foo) '())
;; ()
;; > (intercalate '(a b) '((1 2)))
;; (1 2)
;; > (intercalate '(a b) '((1 2) (3 4) (5 6)))
;; (1 2 a b 3 4 a b 5 6)


;; transpose :: [[a]] -> [[a]]
;; The transpose function transposes the rows and columns of its
;; argument. For example,
;;  transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]
(define (transpose args)
  (if (null? args)
      '()
      (apply zip args)))
;; (define (transpose list-of-lists)
;;   (if (null? list-of-lists)
;;       '()
;;       (apply map (lambda args args) list-of-lists)))

;; > (transpose '())
;; ()
;; > (transpose '((1 2 3 4)))
;; ((1) (2) (3) (4))
;; > (transpose '(((1) (2) (3) (4))))
;; (((1)) ((2)) ((3)) ((4)))
;; > (transpose '((1) (2) (3) (4)))
;; ((1 2 3 4))
;; > (transpose '((1 2 3) (2 3 4) (3 4 5) (4 5 6)))
;; ((1 2 3 4) (2 3 4 5) (3 4 5 6))
;; > (transpose '((1 2 3 4) (2 3 4 5) (3 4 5 6)))
;; ((1 2 3) (2 3 4) (3 4 5) (4 5 6))
;;
;; ; Because I'm using SRFI 1 map, uneven lengths are OK
;; > (transpose '((1 2 3) (4 5 6) (7 8 9 10)))
;; ((1 4 7) (2 5 8) (3 6 9))


;; transpose* : ListOf Lists -> ListOf Lists
;; A more lenient version of transpose, that doesn't stop at the end
;; of the shortest list
(define (transpose* list-of-lists)
  (let loop ((list-of-lists list-of-lists) (answer '()))
    (let ((no-nulls (filter pair? list-of-lists)))
      (if (null? no-nulls)
          (reverse answer)
          (loop (map cdr no-nulls)
                (cons (map car no-nulls)
                      answer))))))
;; (transpose* '((a b c d e f g)
;;               (h i j k l m n)
;;               (o p q r s t u)))
;; (transpose* '((a b c d e f g)
;;               (h i j k l m)
;;               (o p q r s)))
;; (transpose* '((a b c d e)
;;               (h i j k l m)
;;               (o p q r s t u)))
;; (transpose* '((a b c d e f g)
;;               (h i j k)
;;               (o p q r s t u)))

;; subsequences :: [a] -> [[a]]Source
;; The subsequences function returns the list of all subsequences of
;; the argument.
;;  subsequences "abc" == ["","a","b","ab","c","ac","bc","abc"]

(define (subsequences lst)
  (if (null? lst)
      '(())
      (let ((cdr-subs (subsequences (cdr lst))))
        (append cdr-subs
                (map (lambda (x) (cons (car lst) x))
                     cdr-subs)))))
;; (subsequences '())                      ; (())
;; (subsequences '(a))                     ; (() (a))
;; (subsequences '(a b))                   ; (() (b) (a) (a b))
;; (subsequences '(a b c))                 ; (() (c) (b) (b c) (a) (a c) (a b) (a b c))


;; permutations :: [a] -> [[a]]Source
;; The permutations function returns the list of all permutations of
;; the argument.
;;  permutations "abc" == ["abc","bac","cba","bca","cab","acb"]

(define (permutations lst)
;  "Generate a list of all the permutations of the list `lst`"
  (cond ((null? lst) '())
        ((null? (cdr lst)) (list lst))
        (else
         (let ((old-l (permutations (cdr lst))))
           (concatenate (map (lambda (new-l) (insert-at-each (car lst) new-l))
                             old-l))))))

;; > (permutations '())
;; ()
;; > (permutations '(1))
;; ((1))
;; > (permutations '(1 2))
;; ((2 1) (1 2))
;; > (permutations '(1 2 3))
;; ((3 2 1) (3 1 2) (1 3 2) (2 3 1) (2 1 3) (1 2 3))
;; > (length (permutations '(a b c d e f)))
;; 720

(define (insert-at-each item lst)
;  "Returns a list of lists, with `item` inserted into `lst` at different indices"
  (define (join-up head tail)
    (append-reverse head (cons item tail)))
  (let loop ((head-list '()) (tail-list lst) (return-list '()))
    (if (null? tail-list)
        (cons (join-up head-list tail-list) return-list)
      (loop (cons (car tail-list) head-list)
            (cdr tail-list)
            (cons (join-up head-list tail-list) return-list)))))
;; > (insert-at-each 3 '())
;; ((3))
;; > (insert-at-each 9 '(1 2 3 4 5))
;; ((1 2 3 4 5 9) (1 2 3 4 9 5) (1 2 3 9 4 5) (1 2 9 3 4 5)(1 9 2 3 4
;; 5) (9 1 2 3 4 5))


;; init :: [a] -> [a]
;; Return all the elements of a list except the last one. The list
;; must be non-empty.
(define init but-last)


;; inits :: [a] -> [[a]]
;; The inits function returns all initial segments of the argument,
;; shortest first. For example,
;;  inits "abc" == ["","a","ab","abc"]
(define (inits l);; not efficient, but ho hum
  (cons '()
        (map reverse
             (pair-fold cons '() (reverse l)))))

;; tails :: [a] -> [[a]]Source
;; The tails function returns all final segments of the argument,
;; longest first. For example,
;;  tails "abc" == ["abc", "bc", "c",""]
(define (tails l)
  (pair-fold-right cons '(()) l))


;; scan-left :: (a -> b -> a) -> a -> [b] -> [a]
;; (define (scan-left succ zero lst)
;;   (if (null? lst)
;;       (list zero)
;;       (cons zero
;;             (scan-left succ (succ zero (car lst)) (cdr lst)))))
(define (scan-left succ zero lst)
  (loop ((for item (in-list lst))
         (with current zero (succ current item))
         (with answer '() (cons current answer)))
        => (reverse (cons current answer))))

;; (scan-left xcons '() '())
;; '(())
;; scanl (flip (:)) [] []
;; [[]]
;; (scan-left xcons '() '(1 2 3 4))
;; (() (1) (2 1) (3 2 1) (4 3 2 1))
;; scanl (flip (:)) [] [1,2,3,4]
;; [[],[1],[2,1],[3,2,1],[4,3,2,1]]
;; (scan-left + 0 '(1 2 3 4))
;; (0 1 3 6 10)
;; scanl (+) 0 [1,2,3,4]
;; [0,1,3,6,10]

;; scan-right :: (a -> b -> b) -> b -> [a] -> [b]
(define (scan-right succ zero lst)
  ;; TODO: There must be a better way to write this, but I can't
  ;; think of one at the moment.
  (if (null? lst)
      (list zero)
      (let ((rest (scan-right succ zero (cdr lst))))
        (cons (succ (car lst) (car rest)) rest))))

;; (scan-right cons '() '())
;; (())
;; Prelude Data.List> scanr (:) [] []
;; [[]]
;; (scan-right cons '() '(1 2 3 4))
;; ((1 2 3 4) (2 3 4) (3 4) (4) ())
;; Prelude Data.List> scanr (:) [] [1,2,3,4]
;; [[1,2,3,4],[2,3,4],[3,4],[4],[]]
;; (scan-right + 0 '(1 2 3 4))
;; (10 9 7 4 0)
;; Prelude Data.List> scanr (+) 0 [1,2,3,4]
;; [10,9,7,4,0]

(define (scan-left* succ l)
  (scan-left succ (car l) (cdr l)))
(define (scan-right* succ l)
  (scan-right succ (last l) (but-last l)))

(define scanl scan-left)
(define scanr scan-right)
(define scanl1 scan-left*)
(define scanr1 scan-right*)


(define pad
  (case-lambda
    ((list len) (pad list len #f))
    ((list len item)
     (loop ((for item (in-list (reverse list)))
            (for remaining (down-from len (to 0)))
            (for suffix (listing-reverse item)))
           => (if (zero? remaining)
                  suffix
                  (append (make-list remaining item)
                          suffix))))))

;; (pad '(3 2 5) 5) ; (#f #f 3 2 5)
;; (pad '(7 1 3 2 5) 5) ; (7 1 3 2 5)
;; (pad '(8 8 7 1 3 2 5) 5) ;'(7 1 3 2 5)

(define pad-right
  (case-lambda
    ((list len) (pad-right list len #f))
    ((list len item)
     (loop ((for item (in-list list))
            (for remaining (down-from len (to 0)))
            (for prefix (listing item)))
           => (if (zero? remaining)
                  prefix
                  (append prefix (make-list remaining item)))))))
;; (pad-right '(3 2 5) 5) ; '(3 2 5 #f #f)
;; (pad-right '(7 1 3 2 5) 5) ;'(7 1 3 2 5)
;; (pad-right '(8 8 7 1 3 2 5) 5) ;(8 8 7 1 3)


(define (sum l)
  (fold-left + 0 l))
(define (product l)
  (fold-left * 1 l))
(define (maximum l)
  (if (null? l)
      -inf.0
      (foldl1 max l)))
(define (minimum l)
  (if (null? l)
      +inf.0
      (foldl1 min l)))

(define (alternates list)
  (loop ((for item (in-list list))
         (for odds (listing item (if odd?)))
         (for evens (listing item (if (not odd?))))
         (with odd? #t (not odd?)))
        => (values odds evens)))

(define (merge* l1 l2 <)
  (cond ((null? l1)
         (if (null? l2)
             '()
             l2))
        ((null? l2)
         l1)
        ((< (car l1) (car l2))
         (cons (car l1)
               (merge* (cdr l1) l2 <)))
        (else
         (cons (car l2)
               (merge* l1 (cdr l2) <)))))

(define merge
  (case-lambda
    ((l1 l2) (merge* l1 l2 <))
    ((l1 l2 <) (merge* l1 l2 <))))


;; for plists where it can be like
;; key var var var key var key var var var key var key
(define (plist->alist* car? plist)
  ;; assumes head of (car? plist) is true
  (define (rcons a b)
    (cons (reverse a) b))
  (if (null? plist)
      '()
      (let loop ((plist (cdr plist))
                 (current-field (list (car plist)))
                 (return-list '()))
        (cond ((null? plist)
               (reverse
                (if (null? current-field)
                    return-list
                    (rcons current-field return-list))))
              ((car? (car plist))
               (loop (cdr plist)
                     (list (car plist))
                     (rcons current-field return-list)))
              (else
               (loop (cdr plist)
                     (cons (car plist) current-field)
                     return-list))))))

;; (define (unzip list-of-pairs)
;;   (let loop ((pairs list-of-pairs) (cars '()) (cdrs '()))
;;     (if (null? pairs)
;;         (values (reverse cars) (reverse cdrs))
;;         (loop (cdr pairs) (cons (caar pairs) cars)
;;               (cons (cdar pairs) cdrs)))))
(define (unzip* list-of-pairs)
  (loop ((for pair (in-list list-of-pairs))
         (for cars (listing (car pair)))
         (for cdrs (listing (cdr pair))))
        => (values cars cdrs)))

(define (unzip xs)
  ;; from DT`` on #scheme
  (apply values (apply zip xs)))

(define (%distinct? l equal?)
  (cond ((null? l) #t)
        ((member (car l) (cdr l) equal?) #f)
        (else (distinct? (cdr l)))))

(define distinct?
  (case-lambda
    ((list)
     (%distinct? list equal?))
    ((list =?)
     (%distinct? list =?))))

(define (map-if predicate mapper list)
  (map (lambda (x)
         (if (predicate x)
             (mapper x)
             x))
       list))

(define (segregate-by proc list)
  (define buckets (make-eqv-hashtable))
  (for-each (lambda (elem)
              (hashtable-update! buckets
                                 (proc elem)
                                 (lambda (old)
                                   (cons elem old))
                                 '()))
            list)
  (let-values (((keys vals) (hashtable-entries buckets)))
    (vector->list vals)))

(define (insert e l <)
  (cond [(null? l) (list e)]
        [(< e (car l))
         (cons e l)]
        [else
         (cons (car l)
               (insert e (cdr l) <))]))

(define (cross-product/apply f as bs)
  (loop ((for a (in-list as))
         (for result (appending (loop ((for b (in-list bs))
                            (for l (listing (f a b))))
                           => l))))
        => result))

(define (cross-product a b)
  (cross-product/apply cons a b))

;; som variants of unfold

;; unfold* : (s -> bool) -> (s -> values t s) -> s -> [t]
(define (unfold* p n s)
  (if (p s)
      '()
      (call-with-values
          (lambda ()
            (n s))
        (lambda (v next)
          (cons v (unfold* p n next))))))

;; unfold** : (s -> (cons t s) or #f) -> s -> [t]
(define (unfold** n s)
  (let ((next (n s)))
    (if next
        (cons (car next)
              (unfold** n (cdr next)))
        '())))

(define cata fold-right)
(define ana  unfold*)

;; list-ana : (s -> Either () (cons t s)) -> s -> [t]
;; (define (list-ana d s)
;;   (type-case either (d s)
;;     [left (_) '()]
;;     [right (p)
;;            (let ((elem (car p))
;;                  (next-seed (cdr p)))
;;              (cons elem (list-ana d next-seed)))]))

;; ^^ uses my (monad either) library
;; instead, use the version based on unfold*

;; hylo : (s -> bool) -> (s -> values t s) -> (t -> u -> u) -> u -> s -> u -> (s) ->[t]
;; (define (hylo p n f b s)
;;   (cata f b (ana p n s)))
(define (hylo stop? succ combine base seed)
  (define (inner-hylo seed)
    (if (stop? seed)
        base
        (call-with-values
            (lambda ()
              (succ seed))
          (lambda (current next)
            (combine current (inner-hylo next))))))
  (inner-hylo seed))
;; (define (factorial n)
;;   (hylo zero?
;;         (lambda (x)
;;           (values x (- x 1)))
;;         *
;;         1
;;         n))

(define (para a f l)
  (if (null? l)
      a
      (f (car l)
         (cdr l)
         (para a f (cdr l)))))

;; another choices for list-para is
;; 1) to give it the list rather than the car and cdr (this is pair-fold-right)
;; 2) give it a thunk for the third argument to f so it doesn't have
;;    to recurse if not necessary


; I winder if it is worth it to include * versions of
; cata/ana/hylo/para for opposite direction versions, e.g. cata* = fold-left


(define refold hylo)


(define (segments lists)
  ;; like subsequences, but must be consecutive
  ;; not implemented efficiently
  ;; order not defined, currently the empty list does appear more than
  ;; once
  (concatenate (map tails (inits lists))))


(define (snoc l x)
  (append l (list x)))

;; (f accumulator list-val) -> (new-accum new-val)
(define (map-accumulate f a l)
  ;; no the most efficient version I could write
  ;; maybe try it as a fold :P
  (define (inner a l l*)
    (if (null? l)
        (values a (reverse l*))
        (let-values (((a* car*) (f a (car l))))
          (inner a* (cdr l) (cons car* l*)))))
  (inner a l '()))

(define (map-accumulate-right f a l)
  (define (inner a l)
    (if (null? l)
        (values a '())
        (let*-values (((a* cdr*) (inner a (cdr l)))
                      ((a** car*) (f a* (car l))))
          (values a** (cons car* cdr*)))))
  (inner a l))

(define mapAccumL map-accumulate)
(define mapAccumR map-accumulate-right)

(define (partitions lst)
  ;; does a lot of wasted work, can probably write a better dynamic
  ;; programming algorithm for this, or something
  (define (partitions* lst n)
    (append-map (lambda (i)
                  (let-values (((first rest) (split-at lst i)))
                    (if (null? rest)
                        (list (list first))
                        (map (lambda (rest)
                               (cons first rest))
                             (partitions* rest (- n i))))))
                (iota n 1)))
  (partitions* lst (length lst)))

;; (equal? (partitions '()) '())
;; (equal? (partitions '(a)) '(((a))))
;; (equal? (partitions '(a b))
;;         '(((a) (b))
;;           ((a b))))
;; (equal? (partitions '(a b c))
;;         '(((a) (b) (c))
;;           ((a) (b c))
;;           ((a b) (c))
;;           ((a b c))))

)
