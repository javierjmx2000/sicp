;; 2.5

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))
(define (car x) (iter 2 0 x))
(define (cdr x) (iter 3 0 x))
(define (iter n count x)
  (if (= 0 (remainder x n))
      (iter n
            (1+ count)
            (/ x n))
      count))


;; 2.7

(define (upper-bound x) (car x))
(define (lower-bound x) (cdr x))


;; 2.8

(define (sub-internal x y)
  (make-interval (- (lower-bound y) (lower-bound x))
                 (- (upper-bound y) (upper-bound x))))

(define (make-interval a b) (cons a b))
