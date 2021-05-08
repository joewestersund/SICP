#lang sicp

;Church numerals

(define zero   ; notice no parentheses around zero
  (lambda (f) (lambda (x) x)))  ; zero is a function that takes 1 argument (f), ignores it, and returns the identity function.

(define (add-1 n)   ; notice that it does have parentheses
  (lambda (f) (lambda (x) (f ((n f) x)))))  ; add-1 is a function that takes 1 argument. it returns a function that takes 1 argument.

(define one
  (lambda (f) (lambda (x) (f x))))  ; one is a function that takes 1 argument (f). It returns a function that applies f to its argument once. 

(define two
  (lambda (f) (lambda (x) (f (f x)))))  ; two is a function that takes 1 argument (f). It returns a function that applies f to its argument twice. 

(define three
  (lambda (f) (lambda (x) (f (f (f x))))))   ; three is a function that takes 1 argument (f). It returns a function that applies f to its argument 3x. 

(define (test-function x)
  (display "+"))

(define z "z") ; dummy argument

(display "zero = ")
((zero test-function) z)
(newline)(display "one = ")
((one test-function) z)
(newline)(display "two = ")
((two test-function) z)
(newline)(display "three = ")
((three test-function) z)
(newline)(display "add-1 zero = ")
(((add-1 zero) test-function) z)
(newline)(display "add-1 one = ")
(((add-1 one) test-function) z)
(newline)(display "add-1 two = ")
(((add-1 two) test-function) z)
(newline)(display "add-1 three = ")
(((add-1 three) test-function) z)
