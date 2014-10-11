(define (list=? xs ys)
  (cond ((and (null? xs) (null? ys))
         #t)
        ((or (null? xs) (null? ys))
         #f)
        ((= (car xs) (car ys))
         (list=? (cdr xs) (cdr ys)))
        (else #f)))

(define (assert bool)
  (if bool
      (print "ok, ")
      (error "something was false...")))

(define (assert-eq x y)
  (define print-fn (cond ((or (number? x) (string? x))
                          (lambda (x y)
                            (print "\n\nx was: ") (print x) (newline)
                            (print "y was: ") (print y) (newline)))
                         (else (lambda (xs ys)
                                 (print "\n\n  x was: ( ")
                                 (map (lambda (x) (print x) (print ", "))
                                      xs)
                                 (print " )\n  y was: ( ")
                                 (map (lambda (y) (print y) (print ", "))
                                      ys)
                                 (print " )\n\n")))))

  (define eq? (cond ((and (number? x) (number? y)) (lambda (x y) (< (abs (- x y)) 0.0001)))
                    ((and (string? x) (string? y)) string=?)
                    ((and (list? x) (list? y))     equal?)

                    (else (error (string-append "\nmust be the same type\n\tx was: " x "\n\ty was: " y)))))

  (if (eq? x y)
      (print "ok, ")
      (begin
        (print-fn x y)
        (error "not ok!"))))

(define (print-alone x)
  (print "\n\n" x "\n\n"))
