#lang sicp

(define (inc x) (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

((double inc) 5)
(((double double) inc) 5) ; 5 + 2^2 = 9

(((double (double double)) inc) 5) ; 5+ 2^2^2 = 21