#lang sicp

; iterative version
(define (filtered-accumulate combiner null-value filter term a next b)
  (define (iter x result)
    ;(display "iter ")(display x)(display " ")(display result)(newline)
    (cond ((> x b) result)
          ((filter x) (iter (next x) (combiner (term x) result))) ;if x passes filter, put (term a) into result
          (else (iter (next x) result))))    ;if x doesn't pass filter, skip (term a)
  (iter a null-value))

(define (sum term a next b)
  (define (adder x y)
    (+ x y))
  (define (no-filter x) #t)  ; filter that always returns true (allows term to pass)
  (filtered-accumulate adder 0 no-filter term a next b))

(define (filtered-sum filter term a next b)
  (define (adder x y)
    (+ x y))
  (filtered-accumulate adder 0 filter term a next b))

(define (product term a next b)
  (define (multiplier x y)
    (* x y))
  (define (no-filter x) #t)    ; filter that always returns true (allows term to pass)
  (filtered-accumulate multiplier 1 no-filter term a next b))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-test-divisor test-divisor)))))

(define (next-test-divisor n)
  (if (= n 2)
       3
       (+ n 2)))

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m ))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (if (= 1 n)
      #f
      (try-it (+ 1 (random (- n 1))))))

(define (prime? n)
  (fast-prime? n 10))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

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

(define (add-1 x) (+ x 1))

;tests
;(factorial 5)
;(filtered-sum even? identity 1 add-1 10)

(define (sum-of-squares-of-primes a b)
  (define (adder x y) (+ x y))
  (define (add-1 x) (+ x 1))
  (filtered-accumulate adder 0 prime? square a add-1 b))

(sum-of-squares-of-primes 1 5)

(define (greatest-common-divisor x y)
  (define (find-GCD x y start-from best-found)
    ;(display "best found = ")(display best-found)(newline)
    (cond ((> start-from (max x y)) best-found)
          ((and (divides? start-from x) (divides? start-from y)) (find-GCD x y (+ 1 start-from) start-from))  ; start-from is new best-found
          (else (find-GCD x y (+ 1 start-from) best-found))))
  (find-GCD x y 2 1))  ; 1 always works, so initially best-found = 1 and start-from = 2.

; a trickier way to calculate gcd, from the solutions:
(define (gcd m n) 
   (cond ((< m n) (gcd n m)) 
         ((= n 0) m) 
         (else (gcd n (remainder m n))))) 

(define (product-of-relatively-prime x)
  (define (multiplier a b) (* a b))
  (define (relatively-prime c)
    (= (greatest-common-divisor x c) 1))
  (filtered-accumulate multiplier 1 relatively-prime identity 1 add-1 (- x 1)))  ;go to (x-1) because we don't want to include x in product

(product-of-relatively-prime 10)

