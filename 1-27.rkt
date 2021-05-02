#lang sicp

(define (carmichael-test n)
  (do-carmichael-test n 1))

(define (do-carmichael-test n a)
  (cond ((= a n) (display n) (display " passes test but smallest divisor = ") (display (smallest-divisor n)) (newline))
        ((= a (expmod a n n)) (do-carmichael-test n (+ a 1)))
        (else (display n) (display " failed test") (newline))))
              
(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m ))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

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

(define (divides? a b)
  (= (remainder b a) 0))

(carmichael-test 561)
(carmichael-test 1105)
(carmichael-test 1729)
(carmichael-test 2465)
(carmichael-test 2821)
(carmichael-test 6601)
(carmichael-test 6602)
(carmichael-test 6603)
(carmichael-test 6604)

(smallest-divisor 561)



