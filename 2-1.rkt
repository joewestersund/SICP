#lang sicp

; rational numbers
(define (make-rat numerator denominator)
  (let ((g (abs (gcd numerator denominator)))   ; gcd can return a negative number. Take abs
        (sign-corrector (if (< denominator 0) -1 1)))   ; we want the denominator to always be positive. Correct the numerator accordingly
    (cons (* sign-corrector (/ numerator g)) (* sign-corrector (/ denominator g)))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (denom x) (numer y))) (* (denom x) (denom y))))

(define (subtract-rat x y)
  (make-rat (- (* (numer x) (denom y)) (* (denom x) (numer y))) (* (denom x) (denom y))))

(define (mult-rat x y)
  (make-rat (* (numer x) numer y)) (* (denom x) (denom y)))

(define (divide-rat x y)
  (make-rat (* (numer x) denom y)) (* (denom x) (numer y)))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define half (make-rat 1 2))
(define third (make-rat -2 4))

(print-rat third)
(print-rat (add-rat third third))