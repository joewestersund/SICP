#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f times)
  (if (= times 1)
      f
      (compose f (repeated f (- times 1)))))

(define dx 1)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-smoothed f times)
  ((repeated smooth times) f))

(define (square x) (* x x))

((n-smoothed square 1) 5.0)  ; 25 2/3
((n-smoothed square 2) 5.0)  ; 26 1/3
((n-smoothed square 3) 5.0)  ; 27
((n-smoothed square 4) 5.0)  ; 27 2/3

