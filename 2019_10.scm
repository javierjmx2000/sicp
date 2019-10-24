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


;; 2.30

(define (square-tree tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))


;; 2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))


;; 2.32

(define (subsets s)
  (if (null? s)
      (list (list))
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
                          rest)))))


;; 2.33

(define (map p sequence)
  (fold-right (lambda (x y) (cons (p x) y))
              '()
              sequence))

(define (append seq1 seq2)
  (fold-right cons seq2 seq1))

(define (length sequence)
  (fold-right (lambda (x y) (1+ y)) 0 sequence))


;; 2.34

(define (horner-eval x coefficient-sequence)
  (fold-right (lambda (this-coeff higher-terms)
                (+ (* higher-terms) this-coeff))
              0
              coefficient-sequence))


;; 2.35

(define (count-leaves t)
  (fold-right +
              0
              (map (lambda (x)
                     (if (pair? x)
                         (count-leaves x)
                         1))
                   t)))


;; 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (fold-right op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


;; 2.37

(define (dot-product v w)
  (fold-right + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(define (transpose mat)
  (accumulate-n cons (list) mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))


;; 2.39

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) (list) sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) (list) sequence))


;; 2.40

(define (unique-pairs n)
  (append-map (lambda (i)
                (map (lambda (j) (list i j))
                     (iota (- i 1) 1)))
              (iota n 1)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))


;; 2.41

(define (ordered-pairs n)
  (append-map (lambda (i)
                (map (lambda (j) (list j i))
                     (iota (- i 1) 1)))
              (iota n 1)))

(define (ordered-triples n)
  (append-map (lambda (i)
                (map (lambda (p)
                       (append p (list (1+ i))))
                     (ordered-pairs i)))
              (iota (- n 2) 2)))

(define (solution n s)
  (filter (lambda (t) (= s (apply + t)))
          (ordered-triples n)))


;; 2.42

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (append-map
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (iota board-size 1)))
          (queen-cols (-1+ k))))))
  (queen-cols board-size))

(define empty-board (list))

(define (adjoin-position row col positions)
  (cons (list row col) positions))

(define (safe? col positions)
  (define (queens-safe? q1 q2)
    (not (or (= (car q1) (car q2))
             (= (abs (- (car q1) (car q2)))
                (abs (- (cadr q1) (cadr q2)))))))
  (let ((new-queen (car positions))
        (rest (cdr positions)))
    (fold-right (lambda (pos res)
                  (and res (queens-safe? new-queen pos)))
                #t
                rest)))


;; 2.56

(define (deriv exp var)
  (display exp)
  (display ", ")
  (display var)
  (newline)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum 
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define base cadr)

(define exponent caddr)

(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        (else (list '** base exponent))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))
