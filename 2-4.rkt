#lang sicp

(define (cons a b)
  (lambda (m) (m a b)))

(define (car x)
  (x (lambda (y z) y)))

(define (cdr x)
  (x (lambda (y z) z)))

(define test-obj (cons 3 5))

(car test-obj)
(cdr test-obj)