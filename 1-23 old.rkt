#lang sicp

(define (find-next-prime start_from start-time count)
  (cond ((= count 0)
      (display (- (runtime) start-time))
      (display " milliseconds.")
      (newline))
  ((fast-prime? start_from 100)
      ;(display start_from)
      ;(display " is prime")
      ;(newline)
      (find-next-prime (+ start_from 2) start-time (- count 1)))
   (else
       (find-next-prime (+ start_from 2) start-time count))))

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m ))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime-loop start_at multiplier count)
  (cond ((> count 0)
         (display start_at)
         (newline)
         (find-next-prime (+ start_at 1) (runtime) 3)
         (prime-loop (* start_at multiplier) multiplier (- count 1)))))
         

(prime-loop 1000 10 10)


