#lang racket

(define (* x y)
  (mult x y 0)
  )

(define (mult x y a)
  (if (= y 0)
      a
      (if (even? y)
          (mult (double x) (halve y) a)
          (mult x (- y 1) (+ a x))
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
  