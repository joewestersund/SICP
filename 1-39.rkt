#lang sicp

(define (cont-frac-iter n d k)
  (define (frac-step step numerator denominator)
    (if (= step 0)
        (/ numerator denominator)
        (frac-step (- step 1) (n step) (+ (d step) (/ numerator denominator)))))
  (frac-step k 0 1))

; tangent of x in radians
(define (tan-cf x steps)
  (define (n-function step)
    (if (= step 1) x (* -1.0 x x)))
  (define (d-function step)
    (+ 1 (* 2 (- step 1))))
  (cont-frac-iter n-function d-function steps))

(tan-cf 1 100)
(tan-cf 1.5 100)
(tan-cf 2 100)