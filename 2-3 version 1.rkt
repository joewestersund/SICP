#lang sicp
(#%require (only racket/base error))

; rectangles
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

(define (squared-difference a b)
  (square (- a b)))

(define (point-distance p1 p2)
  (sqrt (+ (squared-difference (x-coord p1) (x-coord p2))
           (squared-difference (y-coord p1) (y-coord p2)))))

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

(define (square x) (* x x))

(define tolerance 0.00001)

(define (close-enough? x y)
  (< (abs (- x y)) tolerance))

(define (right-angle? p1 p2 p3)
  (close-enough? (square (point-distance p1 p3)) (+ (square (point-distance p1 p2)) (square (point-distance p2 p3)))))

(define (check-rect? rect)
   (let ((p1 (rect-points rect 1))
        (p2 (rect-points rect 2))
        (p3 (rect-points rect 3))
        (p4 (rect-points rect 4)))
     (and (right-angle? p1 p2 p3) (right-angle? p2 p3 p4) (right-angle? p3 p4 p1) (right-angle? p4 p1 p2))))
     
(define (make-rect p1 p2 p3 p4)   ; points should be the 4 corners of a rectangle, in order around the perimeter
  (let ((r (cons (cons p1 p2) (cons p3 p4))))
    (if (check-rect? r)
        r
        (error "error: points do not make a rectangle\n"))))

(define (rect-points rect point-num)
  (cond ((= point-num 1) (car (car rect)))
        ((= point-num 2) (cdr (car rect)))
        ((= point-num 3) (car (cdr rect)))
        (else (cdr (cdr rect)))))

(define (rect-area rect)
  (let ((p1 (rect-points rect 1))
        (p2 (rect-points rect 2))
        (p3 (rect-points rect 3)))
  (* (point-distance p1 p2)
     (point-distance p2 p3))))

(define (rect-perimeter rect)
  (let ((p1 (rect-points rect 1))
        (p2 (rect-points rect 2))
        (p3 (rect-points rect 3)))
  (* 2 (+ (point-distance p1 p2)
          (point-distance p2 p3)))))

(define p1 (make-point 0 4))
(define p2 (make-point 4 8))
(define p3 (make-point 8 4))
(define p4 (make-point 4 0))

(define rect1 (make-rect p1 p2 p3 p4))

(display "perimeter = ")
(rect-perimeter rect1)
(display "area = ")
(rect-area rect1)



