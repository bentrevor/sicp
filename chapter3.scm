(load "test-suite")
(load "chapter2")
(define true #t)
(define false #f)

(define (prime? n)
  (define (iter n i)
    (cond ((> i (/ n 2)) #t)
          ((= 0 (modulo n i)) #f)
          (else (iter n (+ 1 i)))))
  (cond ((= n 1) #f)
        ((< n 4) #t)
        (else (iter n 2))))

(assert (prime? 3))
(assert (not (prime? 4)))
(assert (prime? 5))
(assert (not (prime? 6)))
(assert (prime? 7))
(assert (not (prime? 8)))
(assert (not (prime? 9)))

(define withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(assert-eq (withdraw 25)
           75)

(assert-eq (withdraw 25)
           50)

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insufficient")))

(define w-1 (make-withdraw 100))
(define w-2 (make-withdraw 100))

(assert-eq (w-1 25)
           75)

(assert-eq (w-2 30)
           70)

(assert-eq (w-2 30)
           40)

(assert-eq (w-1 25)
           50)

;; data-directed ;;
;;;;;;;;;;;;;;;;;;;

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "ins"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "wrong method " m))))
  dispatch)

(define acc-1 (make-account 100))
(define acc-2 (make-account 100))

(assert-eq ((acc-1 'withdraw) 10)
           90)

(assert-eq ((acc-1 'deposit) 50)
           140)

(assert-eq ((acc-2 'withdraw) 50)
           50)

(assert-eq ((acc-2 'deposit) 20)
           70)

(assert-eq ((acc-1 'withdraw) 120)
           20)

(assert-eq ((acc-2 'withdraw) 80)
           "ins")

;; Ex 3.1)

(define (make-accumulator x)
  (let ((acc x))
    (lambda (y)
      (set! acc (+ acc y))
      acc)))

(define A (make-accumulator 5))
(define B (make-accumulator 30))

(assert-eq (A 10)
           15)
(assert-eq (B 10)
           40)
(assert-eq (A 10)
           25)
(assert-eq (B 10)
           50)


(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* product counter) (+ 1 counter))))
  (iter 1 1))

(define (factorial-using-assignment n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ 1 counter))
                 (iter))))
    (iter)))

;; queues ;;
;;;;;;;;;;;;

(define (make-queue)
  (cons '() '()))

(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q x) (set-car! q x))
(define (set-rear-ptr! q x) (set-cdr! q x))

(define (push-queue! q x)
  (let ((new-elt (cons x '())))
    (cond ((empty-queue? q)
           (set-front-ptr! q new-elt)
           (set-rear-ptr! q new-elt))
          (else
           (set-cdr! (rear-ptr q) new-elt)
           (set-rear-ptr! q new-elt)))))

(define (empty-queue? q)
  (null? (front-ptr q)))

(define (pop-queue! q)
  (let ((x (car (front-ptr q))))
    (set-front-ptr! q (cdr (front-ptr q)))
    x))

(define (peek-queue q)
  (if (empty-queue? q)
      '()
      (car (front-ptr q))))


(define q (make-queue))
(assert (empty-queue? q))
(assert-eq '()
           (car q))
(assert-eq '()
           (cdr q))

(push-queue! q 1)
(assert (not (empty-queue? q)))
(assert-eq (peek-queue q)
           1)
(assert-eq '(1)
           (car q))

(push-queue! q 2)
(assert-eq (peek-queue q)
           1)
(assert-eq '(1 2)
           (car q))

(push-queue! q 3)
(assert-eq '(1 2 3)
           (car q))

(assert-eq (pop-queue! q)
           '1)
(assert-eq '(2 3)
           (car q))

(assert-eq (pop-queue! q)
           '2)
(assert-eq '(3)
           (car q))

(define (for-each-except exception procedure list)
  (define (iter items)
    (cond ((null? items) '())
          ((eq? (car items) exception) (iter (cdr items)))
          (else (cons (procedure (car items))
                      (iter (cdr items))))))
  (iter list))

(assert-eq (for-each-except 0 (lambda (x) (* 2 x)) '(0 1 2))
           '(2 4))

(assert-eq (for-each-except 0 (lambda (x) (* 2 x)) '())
           '())

(assert-eq (for-each-except 0 (lambda (x) (* 3 x)) '(0 1 2))
           '(3 6))


;; constraints ;;
;;;;;;;;;;;;;;;;;

;; We can build a network of constraints that are joined by connectors (objects that "hold" a value and
;; participate in a set of constraints). We'll use primitive adder, multiplier, and constant constraints.
;; For example, An adder constraint would ensure that for three values in our system x, y, and sum,
;; that x + y == sum is always true

;; Using the conversion between fahrenheit and celsius temperatures, we can build a constraint network
;; based on the equation:
;;   9 * C = 5 * (32 - F)

;; In the constraint network, when a constraint (ie a node in the network) is
;; "activated" (ie given a value), it awakens all connected constraints and says
;; "hey, I have a value".  Each awakened constraint checks its connectors to see
;; if it has the values it needs to enforce its constraint (ie x + y == sum).
;; If it can enforce the constraint, it will propogate the activation through
;; all of its connectors, except for the connector that activated it in the first place

;; example usage
;; (set-value! C 25 'user)
;; (set-value! F 212 'user) ;; should raise error, expected 77

;; (forget-value! C 'user)  ;; also "unsets" F, because it realizes that there is nothing telling it that it should be 77

;; (set-value! F 212 'user) ;; we can do this now

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u) ;; multiplier verifies that c * w == u
    (multiplier v x u)
    (adder v y f) ;; adder verifies that v + y == f
    (constant 9 w)
    (constant 5 x)
    (constant 32 y))
  'ok)

(define (has-value? connector                    ) (connector 'has-value?))
(define (get-value connector                     ) (connector 'value))
(define (set-value! connector new-value informant) ((connector 'set-value) new-value informant))
(define (forget-value! connector retractor       ) ((connector 'forget-value) retractor))
(define (connect connector new-constraint        ) ((connector 'connect) new-constraint))

(define (inform-about-value constraint)
  (constraint 'i-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'i-lost-my-value))

(define (adder x y sum)
  (define (process-new-value)  ;; called whenever the adder is informed that one of its connectors has a value (notice it doesn't specify _which_ variable got a value)
    ;; each consequent sets the third value according to the values of the other two
    ;; if we don't have two variables with values, don't do anything
    (cond ((and (has-value? x) (has-value? y))
           (set-value! sum
                       (+ (get-value x) (get-value y))
                       self))  ;; the "informant" for these set-value!s is this current adder constraint
          ((and (has-value? x) (has-value? sum))
           (set-value! y
                       (- (get-value sum) (get-value x))
                       self))
          ((and (has-value? y) (has-value? sum))
           (set-value! x
                       (- (get-value sum) (get-value y))
                       self))))
  (define (process-forget-value) ;; called whenever one of the adder's values is forgotten
    (forget-value! sum self)
    (forget-value! x self)
    (forget-value! y self)
    (process-new-value)) ;; run (process-new-value) again because there may be some initially-defined values that we don't want to unset
  (define (self request) ;; self represents this current adder (so this is basically a dispatch method)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else
           (error "unknown request" request))))

  (connect x self)
  (connect y self)
  (connect sum self)

  self)


(define (multiplier x y product)
  (define (process-new-value)
    (cond ((or (and (has-value? x) (= (get-value x) 0))
               (and (has-value? y) (= (get-value y) 0)))
           (set-value! product 0 self))
          ((and (has-value? x) (has-value? y))
           (set-value! product
                       (* (get-value x) (get-value y))
                       self))
          ((and (has-value? product) (has-value? x))
           (set-value! y
                       (/ (get-value product) (get-value x))
                       self))
          ((and (has-value? product) (has-value? y))
           (set-value! x
                       (/ (get-value product) (get-value y))
                       self))))
  (define (process-forget-value)
    (forget-value! product self)
    (forget-value! x self)
    (forget-value! y self)
    (process-new-value))
  (define (self request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else (error "unknown request" request))))

  (connect x self)
  (connect y self)
  (connect product self)

  self)

(define (constant value connector)
  (define (self request)
    (error "unknown request for constant: " request))  ;; raises error for i-have-a-value/i-lost-my-value
  (connect connector self)
  (set-value! connector value self)
  self)

(define (make-connector)
  (let ((value       false)
        (informant   false)
        (constraints '()))
    (define (set-value new-val setter)
      (cond ((not (has-value? self))
             (set! value new-val)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value new-val))
             (error "Contradiction: " (list value new-val)))
            (else 'ignored)))
    (define (forget-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? self)
          (inform-about-value new-constraint))
      'done)
    (define (self req)
      (cond ((eq? req 'has-value?) (if informant true false))
            ((eq? req 'value) value)
            ((eq? req 'set-value) set-value)
            ((eq? req 'forget-value) forget-value)
            ((eq? req 'connect) connect)
            (else (error "unknown request: " req))))
    self))

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

(set-value! C 25 'user)
;; (set-value! F 212 'user) ;; raises Contradiction: (77 212)
(forget-value! C 'user)     ;; also "unsets" F, because it realizes that there is nothing telling it that it should be 77
(set-value! F 212 'user)    ;; we can do this now

;; Ex 3.33)

(define (averager a b c)
  (multiplier a b c))

(define X (make-connector))
(define Y (make-connector))
(define Z (make-connector))

(averager X Y Z)

(set-value! X 3 'user)
(set-value! Y 4 'user)

;; (set-value! Z 5 'user) ;; raises Contradiction: (12 5)
(set-value! Z 12 'user)

;; Ex 3.37)
;; We can define "constraint" versions of normal arithmetic operations that take connectors
;; as arguments and return the related connector.  This lets us write constraints in a more
;; readable "expression-oriented" style

(define (celsius-fahrenheit-converter-v2 x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(define C (make-connector))
(define F (celsius-fahrenheit-converter-v2 C))

(set-value! C 25 'user)
;; (set-value! F 212 'user) ;; raises Contradiction: (77 212)
(forget-value! C 'user)     ;; also "unsets" F, because it realizes that there is nothing telling it that it should be 77
(set-value! F 212 'user)    ;; we can do this now

;; concurrency ;;
;;;;;;;;;;;;;;;;;

;; For our old withdraw method, there is a gap in time between when the program
;; checks the account balance and when it changes the account balance.  When two
;; people are accessing the same account at the same time, this can be the
;; source of bugs.

;; (if (>= balance amount)
;;     (begin (set! balance (- balance amount))
;;            balance))

;; We can use a serializer to define collections of procedures that can't be
;; executed concurrently.  In general, we need a function to execute a group of
;; procedures together, where each procedure accepts no arguments:

;; (parallel-execute (lambda () (set! x (* x x)))  ;; P1
;;                   (lambda () (set! x (+ x 1)))) ;; P2

;; The final value of x will depend on the order that the statements in the
;; procedures are executed.  There are 5 possible outcomes:

;; 101 => P1 sets x to 100 and then P2 increments it to 101
;; 121 => P2 increments x to 11 and then x sets it to x^2=121
;; 110 => P2 changes x from 10 to 11 between the two times that
;;        P1 accesses the value of x during the evaluation of (* x x)
;; 11  => P2 accesses x, then P1 sets x to 100, then P2 sets x
;; 100 => P1 accesses x (twice), then P2 sets x to 11, then P1 sets x

;; We define a serializer that takes a procedure and returns a serialized
;; procedure that has the same effect as the original.  We wrap each of our
;; parallel functions in a serializer:

;; (define s (make-serializer))

;; (parallel-execute (s (lambda () (set! x (* x x))))  ;; P1
;;                   (s (lambda () (set! x (+ x 1))))) ;; P2

;; Now we can only end up with a result of 100 or 121.  So each serializer
;; really just categorizes a function as part of a can't-run-concurrently group.
;; If we had two separate serializers, we could get functions running in
;; parallel:

;; (define s1 (make-serializer))
;; (define s2 (make-serializer))

;; (parallel-execute (s1 (lambda () (set! x (* x x))))   ;; P1
;;                   (s2 (lambda () (set! x (* x x x)))) ;; P2
;;                   (s2 (lambda () (set! x (+ x 1)))))  ;; P3

;; Now, P2 and P3 can't be run at the same time, but either can run at the same
;; time as P1.


;; Our make-account function from earlier gives us back a bank account with a
;; balance, but if two people were using that account at the same time, things
;; could go wrong.  If we serialize access to the accounts, we'll prevent these
;; kinds of timing bugs.  We create each account with its own serializer so that
;; two people can access separate accounts asynchronously, but they can't access
;; the same account at the same time.

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (pprotected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "unknown request in make-account: "
                         m))))
    dispatch))


;; To implement serializers in scheme, we can use a mutex, which is basically a
;; "permission" to modify something that only one "thing" can have at a time.  A
;; mutex can be acquired, which means nobody else is allowed to touch that
;; mutex.  When it is released, it is back up for grabs.

;; A serializer wraps a function and decides when it is allowed to run.  So we
;; need a serialize from (make-serializer) be a function that takes a function
;; as an argument, and returns a serialized version (where "serialized" means
;; the function still behaves the same way, except it won't run at the same time
;; as other functions serialized by the same serializer).

;; So to define a serializer, we have (make-serializer) return a function that
;; acquires a mutex, executes the serialized function, and releases the mutex.

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (f)
      (define (serialized-f . args)
        (mutex 'acquire)
        (let ((val (apply f args)))
          (mutex 'release)
          val))
      serialized-f)))

;; A mutex is an object whose only meaningful state is whether or not it is
;; available, so it can be implemented as a 1-element list with a boolean value:
;; "Is this mutex acquired by someone right now?"  It seems like it would be
;; easier to remember if the boolean answered "Is this mutex available?", but
;; whatever.  A 1-element list is often called a cell.

(define (make-mutex)
  (let ((cell (list #f)))
    (define (self m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (self 'acquire))) ;; retry
            ((eq? m 'release)
             (clear! cell))))
    self))

(define (clear! cell)
  (set-car! cell #f))

(define (test-and-set! cell)
   (if (car cell)
      #t
      (begin (set-car! cell #t)
             #f)))

;; But this can still fail if test-and-set! is not atomic.  Making it atomic is
;; hardware-specific.

;; Ex. 3.47)

;; A semaphore is a generalization of a mutex.  A semaphore allows up to a
;; certain number of processes acquire it concurrently (i.e. a mutex is a
;; semaphore with n=1).

(define (make-semaphore n)
  (define (make-mutexes)
    (define (iter mutexes i)
      (if (= n i)
          mutexes
          (iter (cons (make-mutex) mutexes) (+ i 1))))
    (iter '() 0))
  (let ((cells (make-mutexes n))) ;; cells is a list of n mutexes
    (define (self m)
      (cond ((eq? m 'acquire)
             (if (semaphore-test-and-set! cells)
                 (self 'acquire)))
            ((eq? m 'release)
             (clear-one! cells))))
    self))

(define (semaphore-test-and-set! cells)
  (define (iter remaining-cells)
    (cond ((null? remaining-cells) #t)
          ((caar remaining-cells)  (iter (cdr remaining-cells)))
          (else (begin (set-car! (car remaining-cells) #t)
                       #f))))
  (iter cells))

(define (clear-one! cells)
  (define (iter remaining-cells)
    (cond ((null? remaining-cells) (error "no mutex was able to be released"))
          ((car remaining-cells) (set-car! (car remaining-cells) #f))
          (else (iter (cdr remaining-cells)))))
  (iter cells))


;; streams ;;
;;;;;;;;;;;;;

;; We can use data structures called streams to avoid having to model time in
;; our programs.  When we model the real world with objects that have a
;; time-dependent state, bugs can be caused by the state being updated in the
;; wrong order.  We can give the computer a different idea of "time" by
;; representing changing state as a (possibly infinite) sequence.

;; We can look at functions as an example.  We have a quantity x that varies
;; with time, so we can represent it as a function of time: x(t).  We can think
;; of x as a changing quantity by considering it instant by instant (i.e. each
;; quantity of t).  But we can think of the "entire" function, for all values of
;; t, as a single unchanging entity.

;; We use streams for this.  I can't really tell from the text what the
;; technical definition of a stream is, but it is basically a sequence.  The
;; thing that distinguishes it from a plain old list is that the evaluation of
;; the elements is delayed.

;; We've seen two different ways to perform a computation using a list:

(define (sum-primes-between a b)
  (define (iter i acc)
    (cond ((> i b) acc)
          ((prime? i) (iter (+ 1 i) (+ i acc)))
          (else (iter (+ i 1) acc))))
  (iter a 0))

(define (enumerate-interval a b)
  (if (= a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (sum-primes-between-v2 a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

;; The second version is much less verbose and more readable, but we have to
;; build the whole interval before we can filter it into a new list, and then
;; add it.  So it has a high memory cost in terms of intermediate operations.

;; For example, if we want to find the second prime in the range of 10000 to
;; 10000000, we could use regular lists like this:

;; (car (cdr (filter prime?
;;                   (enumerate-interval 10000 10000000))))

;; But this computes a huge list of numbers, and then takes the second element
;; and throws most of the list away.

;; Using streams, we can get the benefit of the clean functional syntax without
;; the huge memory overhead.  Delayed evaluation means that we only evaluate an
;; element when we actually care about the value of it (i.e. when we try to (car stream)).

;; So we can write code as though we were working with full lists, but behind
;; the scenes, the stream is evaluating each element as we need them.  We'll
;; define stream-equivalents for all of our list functions:

(define stream-null? null?)

(define (stream-ref s n)
  (if (= 0 n)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;; We need the notion of an empty stream that can be identified by stream-null?,
;; and we can just use an empty list for this

(define empty-stream '())

(define (stream-map f s)
  (if (stream-null? s)
      empty-stream
      (cons (f (stream-car s))
            (delay    (stream-map f (stream-cdr s))))))

(define (stream-for-each f s)
  (if (stream-null? s)
      'done
      (begin (f (stream-car s))
             (stream-for-each f (stream-cdr s)))))

(define (print-stream s)
  (stream-for-each print-line s))

(define (print-line x)
  (newline)
  (print x))


;; We need an implementation of streams that can "automatically and
;; transparently interleave the construction of a stream with its use".  Our
;; streams will basically be a list whose cdr is only evaluated at selection
;; time, as opposed to construction time.  We'll use a (delay <exp>) function to
;; do this, so we need a stream constructor that behaves like this:

;; (define (cons-stream a b)
;;   (cons a (delay b)))

;; However, we can't define it as an ordinary procedure because it will
;; automatically try to evaluate its arguments.  So it needs to be a special
;; form somehow.  In this file, I've tried to use cons-stream in the examples,
;; but in order to get them to run, I have to change them to (cons a (delay b)).
;; I've probably left a few of those in here, but really they should all use
;; cons-stream (once it gets implemented).

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

;; The delay function is a promise to evaluate the expression (b) later,
;; whenever it is needed.  To actually evaluate it, we can use a force function.
;; (force (delay <exp>)) will evaluate the expression <exp>.


;; With streams, we can efficiently get the second prime between 10000 and
;; 1000000:

;; (stream-car
;;  (stream-cdr
;;   (stream-filter prime?
;;                  (stream-enumerate-interval 10000 1000000))))

(define (stream-enumerate-interval a b)
  (if (> a b)
      empty-stream
      (cons-stream a (stream-enumerate-interval (+ 1 a) b))))

;; So (stream-enumerate-interval 10000 1000000) gives us

(cons 10000
      (delay (stream-enumerate-interval 10001 1000000)))

(define (stream-filter pred s)
  (cond ((stream-null? s) empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

;; It turns out to be pretty simple to implement delay and force.  We can treat
;; a delayed expression as the body of a function, so delay is really syntactic
;; sugar for wrapping an expression in an anonymous function with no arguments.
;; Then to force an expression, we just need to execute it:

;; (define (delay exp)
;;   (lambda () exp))

;; (define (force exp)
;;   (exp))

;; These functions were already defined for my version of scheme, so this might
;; be something "recently" added to the language.

;; Now we have delay and force implementing the behavior we want, but it's
;; possible for a recursive stream function to force the same delayed object a
;; bunch of times, which can be really expensive.  We can use memoization to
;; avoid repeated computations.  When we memoize a procedure, we save the result
;; to look up later.

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if already-run?
          result
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)))))

;; Then we'd define delay like this:

;; (define (delay exp)
;;   (memo-proc (lambda () exp)))

;; Ex 3.50)
;; extend stream-map to take a collection of streams

(define (stream-map-v2 proc . argstreams)
  (if (null? (car argstreams))
      empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map-v2
              (cons proc (map stream-cdr argstreams))))))

;; funky syntax for variadic functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-border fn)
  (define (func . args)   ;; use the dot for named functions
    (print "\n\nvvvvvvvvvv\n\n")
    (print (apply fn args))
    (print "\n\n^^^^^^^^^^\n\n"))
  func)


(define (add-border-v2 fn)
  (lambda args            ;; don't use parentheses for anonymous functions
    (print "\n\nvvvvvvvvvv\n\n")
    (print (apply fn args))
    (print "\n\n^^^^^^^^^^\n\n")))



;; We can use streams to represent infinite sequences:

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (integers) (integers-starting-from 1))

;; We have to bind this to an anonymous function because we don't have a "real"
;; cons-stream that avoids evaluating the cdr.  Normally, we could just bind it
;; straight to integers:

;; (define integers (integers-starting-from 1))

;; A bunch of the sequences in this file are defined like this, but in the book
;; they are just bound to variables.  When they are just bound to variables,
;; executing this file hangs because it tries to build infinite lists.

(define (integers-starting-from n)
  (cons n (delay (integers-starting-from (+ n 1)))))

(define integers (integers-starting-from 1))

;; So now the list `integers` will be a stream whose car is 1 and whose cdr is
;; the promise to produce the integers starting from 2.  We can use this stream
;; to build other infinite streams:

(define (divisible? x y) (= (modulo x y) 0))

(define (no-sevens)
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

;; We can define more interesting streams too:

(define (fibgen a b)
  (cons a (delay (fibgen b (+ a b)))))

(define fibs (fibgen 0 1))

;; We can implement the sieve of Eratosthenes.  We start with a list of all
;; integers starting at 2.  We filter out the multiples of 2 from the cdr of the
;; list, and end up with a cdr that starts at 3 (the next prime), and doesn't
;; contain any even numbers.  We do this again for the current stream-car (3),
;; and end up with '(2 3 <rest>), where <rest> is a list of integers starting at
;; 5 (the next prime), that doesn't have any multiples of 2 or 3.

(define (sieve s)
  (cons
   (stream-car s)
   (delay (sieve (stream-filter
                  (lambda (x)
                    (not (divisible? x (stream-car s))))
                  (stream-cdr s))))))

(define (primes) (sieve (integers-starting-from 2)))

;; Streams can also be defined "implicitly", which seems to be the same thing as
;; saying they can be defined "recursively".  The fibs and primes sequences were
;; defined by specifying generating procedures to compute each element of the
;; stream.  Instead, we can define a stream in terms of itself:

(define (ones) (cons-stream 1 ones))

;; We can define our sequences of integers and fibonacci numbers this way too if
;; we have a function to get the element-by-element sum of two sequences:

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integers)
  (cons-stream 1 (add-streams ones integers)))

(define (fibs)
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

;; So fibs starts with 0 and 1, and then adds itself to it's own tail, which
;; produces the fibonacci numbers:

;;
;;     1 1 2 3 5 8  13 21 34 ...    = (stream-cdr fibs)
;;     0 1 1 2 3 5  8  13 21 34 ... = fibs
;; 0 1 1 2 3 5 8 13 21 34 ...       = fibs

;; We can get all the powers of 2 by defining a function to scale a stream:

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define powers-of-2
  (cons 1 (delay (scale-stream powers-of-2 2))))

;; Here's how I suspect this works, step by step:

;; p = powers-of-2
;; p*2 = (scale-stream p 2)

;; p = [1, p*2]
;; p = [1, [2, p*2]]
;; p = [1, [2, [4, p*2]]]
;; p = [1, [2, [4, [8, p*2]]]]

;; The tricky part is that p changes every time.  For the next
;; iteration, p = [8, p*2], not the whole built-up stream.

;; We also get a new implementation of listing primes:

(define (primes)
  (cons-stream 2
               (stream-filter prime? (integers-starting-from 3))))

;; Ex 3.54)

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (factorials)
  (cons-stream 1 (mul-streams factorials
                              (integers-starting-from 2))))

;; Ex 3.55

(define (partial-sums s)
  (cons 1 (delay (add-streams s
                              (integers-starting-from 2)))))


;; We can use streams where we would have previously used iteration.  We
;; previously used iteration to implement Newton's method to find
;; increasingly-accurate estimates for the sqrt of a number.  We can generate a
;; stream of infinite guesses instead:

(define (avg x y) (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (avg guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)


;; This gives us a little extra flexibility over plain old iteration.  We can
;; "speed up" a sequence of guesses by tranforming the sequence (where "speed
;; up" means the guesses will be more accurate with fewer iterations).

;; Euler came up with some formula that works well with sequences that are
;; partial sums of alternating series.  We can approximate pi with

;;  pi/4 = 1 - (1/3) + (1/5) - (1/7) + ...

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define (pi-stream)
  (scale-stream (partial-sums (pi-summands 1)) 4))

;; pi-stream will converge on pi, but slowly.  We can use Euler's accelerator to
;; make the estimates closer:

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;; Now we can converge on pi much more quickly with (euler-transform pi-stream)

;; We might want to combine two infinite streams, but we can't append them
;; because it wouldn't be able to find the end of the first sequence.  We can
;; define an interleave function that alternates between two streams:

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))


;; time ;;
;;;;;;;;;;

;; A big focus of this chapter has been how streams deal with time.  We'll
;; revisit the bank account example as one last comparison.  We can create a
;; computational object that will hold a balance, and will let us make
;; withdrawals from it:


(define (make-simplified-withdraw balance)
  (lambda (amount)
    ;; (print "\n\nnew balance: " (- balance amount) "\n\n")
    (set! balance (- balance amount))
    balance))

(define account1 (make-simplified-withdraw 100))

(account1 1)
(account1 10)
(account1 32)
(account1 43)

;; Somebody using this could type in this sequence of withdrawals, and observe
;; the changing balance.  But they have to enter each withdrawal, and this could
;; change every time.  Compare this with a method that builds a stream of
;; consecutive balances based on a given stream of withdrawals:

(define (stream-withdraw balance amount-stream)
  (cons-stream balance
               (stream-withdraw (- balance (stream-car amount-stream))
                                (stream-cdr amount-stream))))

;; This function is pure, because we will always end up with the same stream of
;; balances given a specific initial balance and withdrawal stream.  However, if
;; amount-stream is the stream of inputs from a user, we can get the same
;; behavior as before, but we've avoided assignment/mutability.

;; So this is pretty cool - we can interact with a "stateful" program, even
;; though we are using pure functions whose behavior does not change with time.
;; Really though, we can think of the user's presence as imposing state on the
;; system.

;; Modeling our system using objects is natural because it closely matches the
;; world we live in.  But the mutable state of objects introduces all kinds of
;; potential bugs, which functional languages try to avoid.

;; But functional languages aren't immune to temporal bugs.  For example, we can
;; think of a joint bank account shared by Peter and Paul.  In our
;; object-oriented system, we could see bugs if they try to use/change the same
;; state at the same time.  If we model our account as streams, then Peter and
;; Paul's requests will come from streams.  So if they try to access the account
;; at the same time, we need to merge their streams somehow.

;; It turns out that figuring out how to merge the two streams is not trivial.
;; We still have to guarantee that things happen in the correct order, so we
;; have the same issue as before.





(print "ok!\n\nsuccess!\n")
