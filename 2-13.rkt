#lang sicp

(define (compare-mult-percent i1 i2)
  (define p1 (percent (mul-interval i1 i2)))
  (define p2 (mult-interval-approx-percent i1 i2))
  (display (string-append (number->string p1) " approximation: " (number->string p2) ",   "))
  (define err-amount (- p2 p1))
  (display (string-append (number->string (* 100 (/ err-amount p2))) "% error"))
  (newline))

(define (mult-interval-approx-percent i1 i2)
  (+ (percent i1) (percent i2)))

(define (make-center-percent c p)
  (define w (* c p))
  (make-interval (- c w) (+ c w)))

(define (percent i)
  (/ (width i) (center i)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


(define (compare-mult x y)
  (print-interval (mul-interval x y))
  (print-interval (mul-interval2 x y))
  (display "equal? ")
  (if (interval-equal (mul-interval x y) (mul-interval2 x y))
      (display "y")
      (display "n"))
  (newline))

(define (interval-equal x y)
  (and (= (lower-bound x) (lower-bound y))
       (= (upper-bound x) (upper-bound y))))

(define (mul-interval2 x y)
  (cond
    [(and (above-zero x) (above-zero y))
         (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))]
    [(and (below-zero x) (below-zero y))
         (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y)))]
    [(and (spans-zero x) (spans-zero y))
         (make-interval (min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
                        (max (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))]
    [(and (above-zero x) (below-zero y))
         (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y)))]
    [(and (above-zero y) (below-zero x))
         (make-interval (* (upper-bound y) (lower-bound x)) (* (lower-bound y) (upper-bound x)))]
    [(and (above-zero x) (spans-zero y))
         (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))]
    [(and (above-zero y) (spans-zero x))
         (make-interval (* (upper-bound y) (lower-bound x)) (* (upper-bound y) (upper-bound x)))]
    [(and (below-zero x) (spans-zero y))
         (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y)))]
    [(and (below-zero y) (spans-zero x))
         (make-interval (* (lower-bound y) (upper-bound x)) (* (lower-bound y) (lower-bound x)))]
    [ else (error "condition not handled")]))

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

(define (above-zero interval)
  (> (lower-bound interval) 0))

(define (below-zero interval)
  (< (upper-bound interval) 0))

(define (make-interval lower upper)
  (cons (min lower upper) (max lower upper)))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (interval-to-string i)
  (string-append "(" (number->string (lower-bound i)) " to " (number->string (upper-bound i)) ")"))

(define (print-interval x)
  (display (interval-to-string x))
  (newline))

(define i1 (make-center-percent 10 .05))
(define i2 (make-center-percent 100 .01))
(define i3 (make-center-percent 10 .01))
(define i4 (make-center-percent 100 .05))

(compare-mult-percent i1 i1)
(compare-mult-percent i1 i2)
(compare-mult-percent i1 i3)
(compare-mult-percent i1 i4)
(compare-mult-percent i2 i2)
(compare-mult-percent i2 i3)
(compare-mult-percent i2 i4)
(compare-mult-percent i3 i1)
(compare-mult-percent i3 i2)
(compare-mult-percent i3 i3)
(compare-mult-percent i3 i4)
(compare-mult-percent i4 i3)
(compare-mult-percent i4 i4)
