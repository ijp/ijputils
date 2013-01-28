#!r6rs
(library (ijputils stats)
(export average
        average-list
        kurtosis
        skewness
        variance
        standard-deviation
        ->z-scores
        correlation-coefficient)
(import (rnrs)
        (ijputils common))

(define (average arg1 . args)
  (average-list (cons arg1 args)))

(define (average-list list-of-numbers)
  (let loop ((sum 0) (len 0) (l list-of-numbers))
    (if (null? l)
        (/ sum len)
        (loop (+ sum (car l)) (+ len 1) (cdr l)))))

(define (kurtosis list)
  ;; measures "peakedness"
  ;; higher value means more variance comes from infrequent extreme
  ;; deviations, rather than frequent slight deviations
  (define mean (average-list list))
  (define fourth-moment
    (average-list (map (lambda (x)
                         (expt (- x mean) 4))
                       list)))
  (define var (variance list))
  (- (/ fourth-moment (expt var 2)) 3))

(define (skewness list)
  ;; measure of the asymettry of the probability distribution
  ;; negative means tail on the left side is longer than right
  ;; and the bulk of the values lie to the right of the mean
  ;; positive means tail on right is longer, and bulk of values lie to
  ;; the left
  (define mean (average-list list))
  (define third-moment
    (average-list (map (lambda (x)
                         (expt (- x mean) 3))
                       list)))
  (define var (variance list))
  (/ third-moment (sqrt (expt var 3))))

(define (variance list)
  ;; TODO: make this one-pass
  (let ((avg (average-list list)))
    (average-list (map (lambda (x)
                         (square (- x avg)))
                       list))))

(define (standard-deviation list)
  (sqrt (variance list)))

(define (->z-scores list)
  (define mean (average-list list))
  (define stddev (standard-deviation list))
  (define (->z x)
    (/ (- x mean) stddev))
  (map ->z list))

(define (correlation-coefficient xs ys)
  ;; raw score formula
  (define (deviation-scores ls)
    (define mean (average-list ls))
    (map (lambda (x) (- x mean)) ls))
  (define x-devs (deviation-scores xs))
  (define y-devs (deviation-scores ys))
  (define (sum-cross-products xs ys)
    (sum (map * xs ys)))
  (define covariance
    ;; If I move out the covariance calculation, it should be
    ;; normalised by N, like variance is
    (sum-cross-products x-devs y-devs))
  (define (sum-squares xs)
    (sum (map square xs)))
  (define variance
    (sqrt (* (sum-squares x-devs) (sum-squares y-devs))))
  (/ covariance variance))

)
