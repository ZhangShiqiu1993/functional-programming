#lang planet neil/sicp
(define head car)
(define tail cdr)
(define (same-parity x . xs)
  (let ((mark (remainder x 2)))
    (define (iter xs a)
      (cond ((null? xs) a)
            ((= (remainder (head xs) 2) mark)
             (iter (tail xs) (cons a (head xs))))
            (else
             (iter (tail xs) a))))
    (iter xs x)))
;(define (same-parity . xs)
;  (let ((mark (remainder (head xs) 2)))
;    (filter (lambda (x) (= mark
;                           (remainder x 2)))
;            xs)))
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)