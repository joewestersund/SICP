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

(define (point-distance p1 p2)
  (sqrt (+ (square (x-dist p1 p2))
           (square (y-dist p1 p2)))))

(define (segment-distance seg)
  (point-distance (start-point seg) (end-point seg)))

(define (x-dist p1 p2)
  (- (x-coord p2) (x-coord p1)))

(define (y-dist p1 p2)
  (- (y-coord p2) (y-coord p1)))

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
     
(define (make-rect seg1 seg2)   ; segments should be two adjacent sides of a rectangle
  (let ((r (cons seg1 seg2)))
    (if (check-rect? r)
        r
        (error "error: segments do not make a rectangle\n"))))

(define (rect-segment rect segment-num)
  (if (= segment-num 1)
      (car rect)
      (cdr rect)))
  
(define (rect-points rect point-num)
  (let ((p1 (start-point (rect-segment rect 1)))
        (p2 (end-point (rect-segment rect 1)))
        (p3 (end-point (rect-segment rect 2))))
    (cond ((= point-num 1) p1)
          ((= point-num 2) p2)
          ((= point-num 3) p3)
          (else (make-point (+ (x-coord p1) (x-dist p2 p3)) (+ (y-coord p1) (y-dist p2 p3)))))))

(define (rect-area rect)
  (* (segment-distance (rect-segment rect 1))
     (segment-distance (rect-segment rect 2))))

(define (rect-perimeter rect)
  (* 2 (+ (segment-distance (rect-segment rect 1))
          (segment-distance (rect-segment rect 2)))))

(define p1 (make-point 0 4))
(define p2 (make-point 4 8))
(define p3 (make-point 8 4))
(define p4 (make-point 4 0))

(define seg1 (make-segment p1 p2))
(define seg2 (make-segment p2 p3))

(define rect1 (make-rect seg1 seg2))

(display "perimeter = ")
(rect-perimeter rect1)
(display "area = ")
(rect-area rect1)



