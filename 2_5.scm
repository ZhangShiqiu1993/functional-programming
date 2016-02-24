#lang planet neil/sicp
(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))
(define (car z)
  (factor-expt z 2))
(define (cdr z)
  (factor-expt z 3))

(define (factor-expt dividend factor)
  (if (= 0 (remainder dividend factor))
      (+ 1 (factor-expt (/ dividend factor) factor))
      0))

(define (expt base n)
  (if (= n 0)
      1
      (* base (expt base (- n 1)))))