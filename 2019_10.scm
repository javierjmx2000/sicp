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

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))


;; 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound y) (lower-bound x))
                 (- (upper-bound y) (upper-bound x))))

(define (make-interval a b) (cons a b))


;; 2.10

(define (div-interval x y)
  (if (or (= (lower-bound y) 0)
          (= (upper-bound y) 0))
      (error "Div by zero.")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


;; 2.12

(define (make-center-percent c p)
  (let ((delta (* c p)))
    (make-interval (- c delta) (+ c delta))))

(define (center x)
  (+ (lower-bound x)
     (/ (- (upper-bound x) (lower-bound x))
        2)))

(define (percent x)
  (let ((c (center x)))
    (/ (- (upper-bound x) c)
       c)))


;; 2.17

(define (last-pair l)
  (if (= (length l) 1)
      l
      (last-pair (cdr l))))


;; 2.18

(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l))
              (list (car l)))))


;; 2.19

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? coin-values)
  (= (length coin-values) 0))

(define except-first-denomination cdr)

(define first-denomination car)


;; 2.20

(define (same-parity x . xs)
  (define (iter xs)
    (if (null? xs)
        (list)
        (if (= (remainder x 2) (remainder (car xs) 2))
            (cons (car xs) (iter (cdr xs)))
            (iter (cdr xs)))))
  (cons x (iter xs)))


;; 2.21

(define (square-list items)
  (if (null? items)
      (list)
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list items)
  (map square items))


;; 2.23

(define (for-each proc items)
  (cond ((null? items) (list))
        (else (proc (car items))
              (for-each proc (cdr items)))))


;; 2.27

(define (deep-reverse xs)
  (if (null? xs)
      xs
      (append (deep-reverse (cdr xs))
              (list
               (if (pair? (car xs))
                   (deep-reverse (car xs))
                   (car xs))))))


;; 2.28

(define (fringe x)
  (cond ((null? x) x)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))


;; 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define mobile-left-branch car)

(define (mobile-right-branch m)
  (car (cdr m)))

(define branch-length car)

(define (branch-structure b)
  (car (cdr b)))

(define (branch-weight b)
  (let ((s (structure b)))
    (if (not (pair? s))
        s
        (mobile-total-weight s))))

(define (mobile-total-weight m)
  (+ (branch-weight (mobile-left-branch m))
     (branch-weight (mobile-right-branch m))))

(define (branch-torque b)
  (* (branch-length b)
     (branch-weight b)))

(define (branch-balanced? b)
  (let ((s (branch-structure b)))
    (if (not (pair? s))
        #t
        (mobile-balanced? s))))

(define (mobile-balanced? m)
  (let ((l (mobile-left-branch m))
        (r (mobile-right-branch m)))
    (and (= (branch-torque l)
            (branch-torque r))
         (branch-balanced? l)
         (branch-balanced? r))))
