#lang sicp

; golden ratio: phi^2 = phi + 1
; phi = 1 + 1/phi
(define tolerance 0.0001)

(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          guess
          (try next))))
  (try first-guess))

(fixed-point cos 1)

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1)

; can also use average damping to prevent problems with oscillation
(fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1.0)