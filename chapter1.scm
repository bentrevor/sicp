
(define (double x) (* x 2))
(define (halve x) (/ x 2))
(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (square x) (* x x))

;; 1.2.4)

(define (fast-expt-r b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt-r b (halve n))))
        (else (* b (fast-expt-r b (dec n))))))

(define (fast-expt-i b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0)   a)
        ((even? n) (fast-expt-iter (square b) (halve n) a))
        (else      (fast-expt-iter b (dec n) (* b a)))))

;; Ex 1.17)

(define (fast-*-r x y)
  (cond ((= x 1) y)
        ((even? x) (fast-*-r (halve x) (double y)))
        (else (+ y (fast-*-r (dec x) y)))))

(define (fast-*-i x y)
  (define (iter n)
    (cond ((= 0 x) n)
          ((even? x) (iter (halve x) (double y) n))
          (else      (iter (dec x) y (+ n y)))))
  (fast-*-iter x y 0))

(define (fast-*-iter x y n)
  (cond ((= 0 x) n)
        ((even? x) (fast-*-iter (halve x) (double y) n))
        (else      (fast-*-iter (dec x) y (+ n y)))))

(define (assert-eq x y)
  (if (and (number? x)
           (number? y)) (if (= x y)
                            (print "ok\n")
                            (error (string-append "\nfailed!\n\tx was: " (number->string x) "\n\ty was: " (number->string y))))
           (if (string=? x y)
               (print "ok\n")
               (error (string-append "\nfailed!\n\tx was: " x "\n\ty was: " y)))))


(assert-eq "a" "b")
