#lang planet neil/sicp
(define (cont-frac1 n d k)
  (define (cf-item i)
    (if (= k i)
        (/ (n k) (d k))
        (/ (n k)
           (+ (d k) (cf-item (+ i 1))))))

  (cf-item 1))


(define (cont-frac2 n d k)
  (define (cf-iter i result)
    (if (= i 1)
        (/ (n 1)
           (+ (d 1) result))
        (cf-iter (- i 1)
                 (/ (n i)
                    (+ (d i) result)))))
  (cf-iter k 0))

(cont-frac1 (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)

(cont-frac2 (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)