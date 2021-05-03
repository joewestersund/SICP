#lang sicp

(define tolerance 0.0001)

(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess step-number)
    (let ((next (f guess)))
      (display step-number)(display " guess ")(display guess)(display " f(guess) = ")(display next)(newline)
      (if (close-enough? guess next)
          guess
          (try next (+ 1 step-number)))))
  (try first-guess 1))

; without average damping
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)

; with average damping to prevent problems with oscillation
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2)
