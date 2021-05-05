#lang sicp

(define (newton-method f guess)
  ; find zero point of function f, starting from (f guess)
  (fixed-point (newton-transform f) guess))

(define (newton-transform f)
  (lambda (x) (- x (/ (f x) ((deriv f) x)))))

(define (deriv f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define dx 0.00001)

(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          guess
          (try next))))
  (try first-guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(define a 1)
(define b 0)
(define c 0)

(newton-method (cubic a b c) -2)
