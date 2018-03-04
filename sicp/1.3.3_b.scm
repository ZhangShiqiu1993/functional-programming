#lang planet neil/sicp
(define (average x y)
  (/ (+ x y) 2.0))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;;1.35
(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 1))

