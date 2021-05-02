#lang sicp

(define (find-next-prime start-from start-time count)
  (cond ((= count 0)
       (display (- (runtime) start-time))
       (display " milliseconds.")
        (newline))
  ((prime? start-from)
      ;(display start-from)
      ;(display " is prime")
      ;(newline)
      (find-next-prime (+ start-from 2) start-time (- count 1)))
   (else
       (find-next-prime (+ start-from 2) start-time count))))

(define (prime? n)
  (= n (smallest-divisor n))) 

(define (square x)
  (* x x))

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

(define (next-test-divisor-old n)
  (+ n 1))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime-loop start_at multiplier count)
  (cond ((> count 0)
         (display start_at)
         (newline)
         (find-next-prime (+ start_at 1) (runtime) 3)
         (prime-loop (* start_at multiplier) multiplier (- count 1)))))
         

(prime-loop 1000 10 10)


