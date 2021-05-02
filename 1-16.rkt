#lang racket

(define (exp b n)
  (exp-iter b n 1)
  )

(define (exp-iter b n a)
  (if (= n 0)
      a
      (if (even? n)
          (exp-iter (square b) (/ n 2) a)
          (exp-iter b (- n 1) (* a b))
          )
      )
  )

(define (even? n)
  (= (remainder n 2) 0)
  )

(define (square n)
  (* n n)
  )
