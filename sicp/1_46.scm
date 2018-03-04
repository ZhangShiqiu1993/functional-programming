#lang planet neil/sicp
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve)
         (improve guess)))))

(define (fixed-point f guess)
  (define tolerance 0.000000001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define good?
    (lambda (guess) (close-enough? guess (f guess))))
  ((iterative-improve good? f) guess))

(define (sqrt x)
  (define (average x y) (/ (+ x y) 2.0))
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))