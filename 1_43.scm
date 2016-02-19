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
         (compose f (repeat f (- n 1)) ))))

(define (repeated f n)
  (define (big-enough? x) (>= (/ x 2) n))
  (define (iter k result)
    (cond ((= k 0) result)
          ((big-enough? k) (iter (/ k 2) (f (f result))))
          (else (iter (- k 1) (f result)))))
  (lambda (x) (iter n x)))

((repeat square 2) 5)
((repeated square 2) 5)