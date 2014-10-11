(load "test-suite")

;; Ex 2.2)

(define (make-segment start end)
  (cons start end))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define p1 (make-point 1.2 3.4))
(define p2 (make-point 5.6 7.8))
(define seg (make-segment p1 p2))

(define (midpoint seg)
  (define (avg x y)
    (/ (+ x y)
       2))
  (define (mid-x seg)
    (avg (x-point (start-segment seg))
         (x-point (end-segment seg))))
  (define (mid-y seg)
    (avg (y-point (start-segment seg))
         (y-point (end-segment seg))))
  (make-point (mid-x seg)
              (mid-y seg)))


(assert-eq (x-point (midpoint seg))
           3.4)
(assert-eq (y-point (midpoint seg))
           5.6)

;; 2.4)

(define (cons-v2 x y)
  (lambda (m) (m x y)))

(define (car-v2 z)
  (z (lambda (p q) p)))

(define (cdr-v2 z)
  (z (lambda (p q) q)))

(assert-eq (car-v2 (cons-v2 5 6))
           5)

(assert-eq (cdr-v2 (cons-v2 7 8))
           8)

;; 2.5)

(define (log-base-2 n)
  (/ (log n)
     (log 2)))

(define (log-base-3 n)
  (/ (log n)
     (log 3)))

(define (cons-v3 a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car-v3 pair)
  (if (= 0 (modulo pair 3))
      (car-v3 (/ pair 3))
      (log-base-2 pair)))

(define (cdr-v3 pair)
  (if (= 0 (modulo pair 2))
      (cdr-v3 (/ pair 2))
      (log-base-3 pair)))

(assert-eq (log-base-2 8) 3)
(assert-eq (log-base-2 32) 5)
(assert-eq (log-base-2 256) 8)

(assert-eq (log-base-3 81) 4)
(assert-eq (log-base-3 243) 5)


(assert-eq (car-v3 (cons-v3 5 6))
           5)

(assert-eq (cdr-v3 (cons-v3 7 8))
           8)

;; 2.17)
(define (last-pair xs)
  (define (iter xs count)
    (if (null? (cdr xs))
        xs
        (iter (cdr xs) (+ 1 count))))
  (iter xs 0))

(assert-eq (car (last-pair '(1 2 3 4 5)))
           5)

;; 2.18)
(define (reverse xs)
  (define (iter xs tail)
    (if (null? (cdr xs))
        (cons (car xs) tail)
        (iter (cdr xs) (cons (car xs) tail))))

  (if (null? xs)
      xs
      (iter xs '())))

(assert-eq (reverse '())
           '())

(assert-eq (reverse '(1 2 3 4 5))
           '(5 4 3 2 1))

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(assert-eq '(2 4 6 8 10)
           (scale-list '(1 2 3 4 5) 2))

(assert-eq '(20 40 60 80 100)
           (scale-list '(2 4 6 8 10) 10))

(define (scale-list-v2 items factor)
  (map (lambda (x) (* x factor))
       items))

(assert-eq (scale-list    '(1 2 3 4 5) 10)
           (scale-list-v2 '(1 2 3 4 5) 10))

;; Ex 2.21)

(define (square-list xs)
  (map (lambda (x) (* x x)) xs))

(define (square-list-v2 xs)
  (if (null? xs)
      '()
      (cons (* (car xs) (car xs)) (square-list-v2 (cdr xs))))
  )

(assert-eq '(1 4 9 16 25)
           (square-list '(1 2 3 4 5)))

(assert-eq '(1 4 9 16 25)
           (square-list-v2 '(1 2 3 4 5)))

(assert-eq (square-list '(13 345 567 698))
           (square-list-v2 '(13 345 567 698)))

;; Ex 2.27)

(define (deep-reverse xs)
  (if (pair? xs)
      (reverse (map deep-reverse xs))
      xs))

(assert-eq (car (deep-reverse '((1 2 3) (4 5))))
           '(5 4))

(assert-eq (cadr (deep-reverse '((1 2 3) (4 5))))
           '(3 2 1))

(assert-eq (cadr (cadr (deep-reverse '(((1 2) 3) (4 5)))))
           '(2 1))

;; Ex 2.28)

(define (init xs)
  (reverse (cdr (reverse xs))))

(define (concat xs ys)
  (define (iter xs ys)
    (cond ((null? xs) ys)
          ((not (pair? xs)) (cons xs ys))
          (else (concat (init xs) (cons (car (last-pair xs)) ys)))))

  (iter xs ys))

(assert-eq (concat '(1 2 3 4) '(5 6 7 8))
           '(1 2 3 4 5 6 7 8))

(define (fringe tree)
  (if (pair? tree)
      (concat (fringe (car tree)) (fringe (cdr tree)))
      tree))

(define tree (list (list 1 2) (list 3 4)))
(assert-eq (fringe tree)
           '(1 2 3 4))

(assert-eq (fringe (list tree tree))
           '(1 2 3 4 1 2 3 4))


(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* factor tree))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(assert-eq (fringe (scale-tree tree 3))
           '(3 6 9 12))

(define (scale-tree-v2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-v2 sub-tree factor)
             (* factor sub-tree)))
       tree))

(assert-eq (fringe (scale-tree-v2 tree 3))
           '(3 6 9 12))


(define (filter op seq)
  (cond ((null? seq) '())
        ((op (car seq)) (cons (car seq) (filter op (cdr seq))))
        (else (filter op (cdr seq)))))

(assert-eq (filter odd? '(1 2 3 4 5 6 7))
           '(1 3 5 7))


;; (define (wrong-reduce op acc seq)
;;   (if (null? seq)
;;       acc
;;       (reduce op (op (car seq) acc) (cdr seq))))
;;
;; this doesn't work because it applies the operation in the wrong order, e.g.
;;
;; $ (wrong-reduce cons '() '(1 2 3 4 5))
;; > (5 4 3 2 1)

(define (reduce op acc seq)
  (if (null? seq)
      acc
      (op (car seq)
          (reduce op acc (cdr seq)))))

(assert-eq (reduce + 0 '(1 2 3 4 5))
           15)

(assert-eq (reduce * 1 '(1 2 3 4 5))
           120)

(assert-eq (reduce cons '() '(1 2 3 4 5))
           '(1 2 3 4 5))

(define (memq x xs)
  (cond ((null? xs) #f)
        ((eq? x (car xs)) xs)
        (else (memq x (cdr xs)))))

(assert-eq (memq 2 '(1 2 3))
           '(2 3))

(assert (not (memq 8 '(1 2 3))))


;; Sets
;;
;; union-set        => set of all members that belong to either set
;; intersection-set => set of all members that belong to both sets
;; elt-of-set?      => check membership
;; adjoin-set       => add member to set (if it isn't already one)


;; unordered lists

(define (elt-in-set? x xs)
  (cond ((null? xs) #f)
        ((equal? x (car xs)) #t)
        (else (elt-in-set? x (cdr xs)))))

(assert (elt-in-set? 1 '(1 2 3)))
(assert (not (elt-in-set? 4 '(1 2 3))))

(define (adjoin-set x xs)
  (if (elt-in-set? x xs)
      xs
      (cons x xs)))

(assert-eq (adjoin-set 1 '(2 3))
           '(1 2 3))

(assert-eq (adjoin-set 1 '(1 2 3))
           '(1 2 3))

(define (intersection-set xs ys)
  (cond ((or (null? xs) (null? ys)) '())
        ((elt-in-set? (car xs) ys) (cons (car xs) (intersection-set (cdr xs) ys)))
        (else (intersection-set (cdr xs) ys))))

(assert (not (elt-in-set? 1 (intersection-set '(1 2 3) '(2 3 4)))))
(assert (elt-in-set? 2 (intersection-set '(1 2 3) '(2 3 4))))
(assert (elt-in-set? 3 (intersection-set '(1 2 3) '(2 3 4))))
(assert (not (elt-in-set? 4 (intersection-set '(1 2 3) '(2 3 4)))))

;; Ex 2.59)

(define (union-set xs ys)
  (cond ((null? xs) ys)
        ((null? ys) xs)
        ((elt-in-set? (car xs) ys) (union-set (cdr xs) ys))
        (else (cons (car xs) (union-set (cdr xs) ys)))))

(assert (elt-in-set? 1 (union-set '(1 2 3) '(2 3 4))))
(assert (elt-in-set? 2 (union-set '(1 2 3) '(2 3 4))))
(assert (elt-in-set? 3 (union-set '(1 2 3) '(2 3 4))))
(assert (elt-in-set? 4 (union-set '(1 2 3) '(2 3 4))))


;; (balanced) binary trees
;; the trees will be implemented as lists with (car tree) holding the content and (cdr tree)
;; holding the left/right branches

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (elt-of-btree? x tree)
  (cond ((null? tree)       #f)
        ((= x (entry tree)) #t)
        ((< x (entry tree)) (elt-of-btree? x (left-branch tree)))
        ((> x (entry tree)) (elt-of-btree? x (right-branch tree)))))

(define (adjoin-btree x tree)
  (cond ((null? tree) (make-tree x '() '()))
        ((= x (entry tree)) tree)
        ((< x (entry tree)) (make-tree (entry tree) (adjoin-btree x (left-branch tree)) (right-branch tree)))
        ((> x (entry tree)) (make-tree (entry tree) (left-branch tree) (adjoin-btree x (right-branch tree))))))

;;      5
;;   3     7
;; 1   4 6   8


(define btree (make-tree 5
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    (make-tree 4 '() '()))
                         (make-tree 7
                                    (make-tree 6 '() '())
                                    (make-tree 8 '() '()))))

(assert (elt-of-btree? 5 btree))
(assert (elt-of-btree? 3 btree))
(assert (elt-of-btree? 7 btree))
(assert (elt-of-btree? 8 btree))
(assert (not (elt-of-btree? 0 btree)))
(assert (not (elt-of-btree? 9 btree)))
(assert (not (elt-of-btree? 100 btree)))

(assert (elt-of-btree? 100 (adjoin-btree 100 btree)))
