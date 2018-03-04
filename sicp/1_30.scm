#lang planet neil/sicp
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ (term a)
                 result))))
  (iter a 0))

;;以下是辅助代码，仅供测试
(define (cube x) (* x x x))
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
;;(integral cube 0 1 0.0001)