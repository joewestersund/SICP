#lang sicp

(define (cont-frac n d k)
  (define (frac-step step)
    (if (> step k)
        0
        (/ (n step) (+ (d step) (frac-step (+ 1 step))))))
  (frac-step 1))

(define (cont-frac-iter n d k)
  (define (frac-step step numerator denominator)
    (if (= step 0)
        (/ numerator denominator)
        (frac-step (- step 1) (n step) (+ (d step) (/ numerator denominator)))))
  (frac-step k 0 1))


(define (frac-loop a b frac-function)
  (cond ((<= a b)
      (display "frac-loop ")
       (display a)
       (display " = ")
       (let ((v1 (frac-function (lambda (x) 1.0) (lambda (x) 1.0) a)))
         (display v1)
         (newline))
      (frac-loop (+ 1 a) b frac-function))
      (else 0)))

(frac-loop 1 20 cont-frac)
(frac-loop 1 20 cont-frac-iter)

; linear recursion version takes 12 iterations to converge to 1/phi to within 4 decimal places (0.6180).
; iterative version takes 12 iterations as well, calculation appears to be exactly the same.
