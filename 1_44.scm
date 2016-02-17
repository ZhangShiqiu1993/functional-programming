#lang planet neil/sicp
(define (square x)(* x x))

(define (double f)(lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeat f n)
  (cond ((= n 1)(lambda (x) (f x)))
        ((even? n)
         (double (repeat f (/ n 2))))
        (else
         (compose f (repeat f (- n 1))))))

(define dx 0.000001)
(define (avg-3 a b c)
  (/ (+ a b c) 3))
(define (smooth f)
  (lambda (x)
    (avg-3 (f (- x dx))
           (f x)
           (f (+ x dx)))))

(define (smooth-n n)
  (repeat smooth n))