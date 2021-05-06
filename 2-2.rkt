#lang sicp

(define (make-point x y)
  (cons x y))

(define (x-coord point)
  (car point))

(define (y-coord point)
  (cdr point))

(define (make-segment point1 point2)
  (cons point1 point2))

(define (start-point segment)
  (car segment))

(define (end-point segment)
  (cdr segment))

(define (print-point point)
  (display "(")
  (display (x-coord point))
  (display ",")
  (display (y-coord point))
  (display ")")
  (newline))

(define (display-segment segment)
  (display "start point: ")
  (print-point (start-point segment))
  (display "end point: ")
  (print-point (end-point segment)))

(define (average a b)
  (/ (+ a b) 2))
 
(define (midpoint-segment segment)
  (make-point (average (x-coord (start-point segment)) (x-coord (end-point segment)))
              (average (y-coord (start-point segment)) (y-coord (end-point segment)))))

(define p1 (make-point 1 0))
(define p2 (make-point 2 1))
(define seg (make-segment p1 p2))
(print-point (midpoint-segment seg))

