#lang sicp

(define (sum term a next b)
  ;iterative version of sum
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (cube x)
  (* x x x))

(define (identity x)
  x)

(define (flat-line x)
  1)

(define (add-1 x) (+ 1 x))

(sum flat-line 0 add-1 1)
    