#lang sicp

(define (cont-frac-iter n d k)
  (define (frac-step step numerator denominator)
    (if (= step 0)
        (/ numerator denominator)
        (frac-step (- step 1) (n step) (+ (d step) (/ numerator denominator)))))
  (frac-step k 0 1))

(define (euler-e-estimator steps)
  (define (d-function step)
    (cond ((or (= (remainder step 3) 0) (= (remainder step 3 ) 1)) 1.0)
          (else (* 2 (ceiling (/ step 3.0))))))
  (+ 2 (cont-frac-iter (lambda (x) 1.0) d-function steps)))

(euler-e-estimator 100)