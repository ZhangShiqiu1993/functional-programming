#lang planet neil/sicp
(define (average x y)
  (/ (+ x y) 2.0))

(define tolerance 0.000000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define g
  (lambda (x) (/ (log 1000)(log x))))

(fixed-point (lambda (x) (average x (g x))) 2.0)
(newline)
(fixed-point g 2.0)