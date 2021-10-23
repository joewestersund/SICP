#lang sicp

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))

(define (make-interval lower upper)
  (cons lower upper))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (print-interval x)
  (display "(")
  (display (lower-bound x))
  (display " to ")
  (display (upper-bound x))
  (display ")")
  (newline))

(define i1 (make-interval 3 5))
(define i2 (make-interval 11 13))

(print-interval (mul-interval i1 i2))
(print-interval (div-interval i1 i2))



