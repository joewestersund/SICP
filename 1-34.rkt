#lang sicp

(define (f g)
  (display "f = ")(display f)(newline)
  (display "g = ")(display g)(newline)
  (g 2))

(define (test x)
  (let ((x 3)
    (y (+ x 2)))
    (display "x = ")(display x)(newline)
    (display "y = ")(display y)(newline)
    (* x y)))

;(test 2)

(define (square x) (* x x))

(f square)

(f f)
