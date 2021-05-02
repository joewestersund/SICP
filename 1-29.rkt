#lang sicp

(define (sum term a next b)
  ;(display "adding ") (display (term a)) (newline)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (even? x)
  (= (remainder x 2) 0))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (simpson-integral f a b n)
  (define (simp-prefix k)
    ;(display "simp-term ")
    ;(display k)
    ;(newline)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))
  (define (simp-term k)
    (* (simp-prefix k) (f (+ a (* k (/ (- b a) n))))))
  (define (simp-next k)
    ;(display "simp-next ")
    ;(display k)
    ;(newline)
    (+ k 1))
  ;(display "h = ")
  ;(display (/ (- b a) n))
  ;(newline)
  (* (/ (- b a) (* n 3)) (sum simp-term 0 simp-next n)))

(define (cube x)
  (* x x x))

(define (identity x)
  x)

(define (flat-line x)
  1)

(define (num-steps) 1000)

(define (step-size n-steps start end)
  (/ (- end start) n-steps))

(integral cube 0 1 (step-size (num-steps) 0 1))
(simpson-integral cube 0 1 (num-steps))


  
  
  