#lang planet neil/sicp
(define make-interval cons)
(define lower-bound car)
(define upper-bound cdr)

(define (cross-zero? x)
  (<= (* (lower-bound x) (lower-bound x)) 0))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (if (cross-zero? y)
      (error "区间横跨0")
      (mul-interval x
                     (make-interval (/ 1.0 (upper-bound y))
                                    (/ 1.0 (lower-bound y))))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))
(define (width x)
  (let ((length (- (upper-bound x)
                   (lower-bound x))))
    (/ length 2.0)))








(define a (make-interval 4 5))
(define b (make-interval 1 2))
(define r-sub (sub-interval a b))
(define r-add (add-interval a b))
(define r-mul (mul-interval a b))
(define r-div (div-interval a b))
(define c (make-interval -1 1))