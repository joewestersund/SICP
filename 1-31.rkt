#lang sicp

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))


(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial x)
  (define (add-1 x) (+ x 1))
  (define (identity x) x)
  (product identity 1 add-1 x))

(define (factorial-iter x)
  (define (add-1 x) (+ x 1))
  (define (identity x) x)
  (product-iter identity 1 add-1 x))

(define (pi-product num-terms)
  (define (next-term k)
    ; k should start at 1 and go up
    (/ (+ 2 (* 2 (floor (/ k 2)))) (+ 3 (* 2 (floor (/ (- k 1) 2))))))
  (define (add-1 x) (+ x 1))
  (* 4.0 (product-iter next-term 1 add-1 num-terms)))

;(define (num) 5)

;(factorial (num))
;(factorial-iter (num))

(pi-product 100000)