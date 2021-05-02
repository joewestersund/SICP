#lang racket

(define (fib row col)
  (if (or (= row 1) (= col 1) (= col row))
      1
      (+ (fib (- row 1) (- col 1)) (fib (- row 1) col))
      )
  )

(define (print-fib-line row col)
  (display (fib row col))
  (display " ")
  (when (< col row)
      (print-fib-line row (+ 1 col))
      )
  )

(define (print-fib-pyramid row max-row)
  (print-spaces (- max-row row))
  (print-fib-line row 1)
  (displayln "")
  (when (< row max-row)
      (print-fib-pyramid (+ 1 row) max-row)
))

(define (print-spaces n)
  (when (> n 0)
    (display " ")
    (print-spaces (- n 1))
    )
  )