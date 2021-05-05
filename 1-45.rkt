#lang sicp

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f times)
  (if (= times 1)
      f
      (compose f (repeated f (- times 1)))))

(define tolerance 0.0001)

(define error-value -1)

(define (fixed-point f first-guess max-iterations)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess iterations-left)
    (if (= iterations-left 0)
        error-value
        (let ((next (f guess)))
          (if (close-enough? guess next)
              guess
              (try next (- iterations-left 1))))))
  (try first-guess max-iterations))

(define (n-th-root n x guess damping-repeats)
  (fixed-point ((repeated average-damp damping-repeats) (lambda (y) (/ x (expt y (- n 1))))) guess 1000))

(define (test-damping-repeats x first-guess max-root max-repeats)
  (define (iter n damping-repeats)
    (cond ((> damping-repeats max-repeats)(display n)(display " root didn't converge with ")(display (- damping-repeats 1))(display " damping repeats")(newline)(iter (+ n 1) 1))
          ((> n max-root) (newline)(display "done")(newline))
          (else (let ((result (n-th-root n x first-guess damping-repeats)))
                  (cond ((= result error-value) (iter n (+ damping-repeats 1)))
                      (else (display n)(display " root converged to ")(display result)(display " with ")(display damping-repeats)(display " damping repeats")(newline)
                            (iter (+ n 1) 1)))))))
  (iter 1 1))

(test-damping-repeats 64.0 1.0 20 4)