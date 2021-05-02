#lang racket

(define (runtime)
  (current-milliseconds))

(define (find-next-prime start_from start-time count)
  (cond ((= count 0)
      (display (- (runtime) start-time))
      (display " milliseconds.")
      (newline))
  ((fast-prime-fermat? start_from 10)
      (cond ((fast-prime-miller-rabin? start_from 10)
             (display start_from)
             (display " is prime")
             (newline))
            (else (display start_from)
             (display " is not prime, but fooled the fermat test.")
             (newline)))
      (find-next-prime (+ start_from 2) start-time (- count 1)))
   (else
    ;(display start_from)
    ;(display " is not prime")
       (find-next-prime (+ start_from 2) start-time count))))

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (nontrivial-square-root a n sq)
  (if (and
       (not (= a 1))
       (not (= a (- n 1)))
       (= sq 1))
      0
      sq))

(define (squaremod-with-check x n)
  (nontrivial-square-root x n (remainder (square x) n)))

(define (miller-rabin-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (squaremod-with-check (miller-rabin-expmod base (/ exp 2) m) m))
        (else (remainder (* base (miller-rabin-expmod base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
    ;(display "trying ")
    ;(display a)
    ;(display " for prime test of ")
    ;(display n)
    ;(newline)
    (= (miller-rabin-expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime-miller-rabin? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime-miller-rabin? n (- times 1)))
        (else false)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m ))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime-fermat? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime-fermat? n (- times 1)))
        (else false)))


(find-next-prime 3 (runtime) 500)


