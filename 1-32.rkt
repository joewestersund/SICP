#lang sicp

; linear recursion version
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

; iterative version
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (define (adder x y)
    (+ x y))
  (accumulate-iter adder 0 term a next b))

(define (product term a next b)
  (define (multiplier x y)
    (* x y))
  (accumulate-iter multiplier 1 term a next b))

(define (factorial x)
  (define (add-1 x) (+ x 1))
  (define (identity x) x)
  (product identity 1 add-1 x))

(define (cube x)
  (* x x x))

(define (identity x)
  x)

(define (flat-line x)
  1)

(define (add-1 x) (+ 1 x))

; tests
(sum identity 0 add-1 9)

(factorial 5)