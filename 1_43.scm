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
  (define (could-double? x)
    (<= (* x 2) n))
  (define (iter k g)
    (cond ((= k n) g)
          ((could-double? k)(iter (* k 2) (double g)))
          (else
           (compose f (iter (- k 1) g)))))
  (iter 1 f))

((repeated square 2) 5)