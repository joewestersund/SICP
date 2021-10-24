#lang sicp

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

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
  (cond [(spans-zero y)
         (error (string-append "cannot calculate div-interval. Interval " (interval-to-string y) " spans zero."))]  
      [else (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))]
   )
 )

(define (spans-zero interval)
  (<= (* (lower-bound interval) (upper-bound interval)) 0 ))
  
(define (make-interval lower upper)
  (cons lower upper))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (interval-to-string i)
  (string-append "(" (number->string (lower-bound i)) " to " (number->string (upper-bound i)) ")"))

(define (print-interval x)
  (display (interval-to-string x))
  (newline))

(define i1 (make-interval 1 2))
(define i2 (make-interval 1 3))
(define i3 (make-interval 1 -3))

(print-interval (div-interval i1 i2))
(print-interval (div-interval i2 i1))
(print-interval (div-interval i3 i1))
(print-interval (div-interval i1 i3))


