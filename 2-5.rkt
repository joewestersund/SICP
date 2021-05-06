#lang sicp

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (divides? x y)
  (= 0 (remainder y x)))

(define (car x)
  (if (divides? 3 x)
      (car (/ x 3))   ; remove all the threes out of it
      (log x 2)))   ; all the threes removed, so now take log

(define (cdr x)
  (if (divides? 2 x)
      (cdr (/ x 2))
      (log x 3)))

(define pair (cons 5 7))

(display "first = ")
(car pair)
(display "second = ")
(cdr pair)