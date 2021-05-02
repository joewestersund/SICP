#lang racket

(define (* x y)
  (if (= y 0)
      0
      (if (even? y)
          (* (double x) (halve y))
          (+ x (* x (- y 1)))
          )
      )
  )

(define (even? x)
  (= (remainder x 2) 0)
  )

(define (halve x)
  (/ x 2)
  )

(define (double x)
  (+ x x)
  )
  