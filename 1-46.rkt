#lang sicp

(define (iterative-improve good-enough? improve-guess)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve-guess guess))))
  (lambda (guess) (iter guess)))  ; could also just return the iter function

(define (average x y)
  (/ (+ x y) 2))

(define (improve-guess-sqrt x)
  (lambda (guess) (average guess (* guess (/ x (* guess guess))))))

(define tolerance 0.000000001)

(define (good-enough-sqrt? x)
  (lambda (y) (< (abs (- (* y y) x)) tolerance)))

(define (square-root x guess)
  ((iterative-improve (good-enough-sqrt? x) (improve-guess-sqrt x)) guess))

(square-root 49.0 1)
