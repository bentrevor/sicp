(load "test-suite")
(load "chapter2")
(load "chapter3")

;; An evaluator (or interpreter) for a language is a procedure that performs the
;; actions required to evaluate whatever expression was passed to it as an
;; argument.  Since the evaluator itself is just another program, we can think
;; of any program as being the evaluator for some DSL.  For instance, an
;; implementation of a chess game in Ruby defines a chess DSL that provides
;; abstractions over different entities of a chess game.  Running a chess game
;; is just evaluating that DSL.

;; We will implement a Scheme metacircular evaluator, which is an evaluator
;; written in the same language it is evaluating.  Basically, we will be
;; implementing the environment model of computation using Scheme. In the
;; environment model, an environment is a list of frames along with a reference
;; to the enclosing environment.  A frame is a list of variable bindings.  When
;; asking for the value of a variable in an environment, the evaluator finds the
;; first frame that has a binding for that variable and returns that value.

;; Evaluating lisp (or any language?) requires us to define two rules:

;; 1) To evaluate a combination (a compound expression that is not a special
;; form), evaluate the subexpressions and then apply the value of the operator
;; subexpression to the values of the operand subexpressions.

;; 2) To apply a compound procedure to a set of arguments, evaluate the body of
;; the procedure in a new environment.  The new environment will bind variables
;; for all the parameters that are passed in.

;; In pseudocode:

;; def eval(exp, env)
;;   tail = exp[1..-1]
;;   proc = exp[0]
;;   args = tail.each do |subexp|
;;     eval(subexp, env)
;;   end

;;   apply(proc, args)
;; end

;; def apply(proc, args)
;;   env = Env.new(args)
;;   exp = Exp.new(proc, args)

;;   eval(exp, env)
;; end

;; Our evaluator will essentially reduce our program down to primitive values
;; and procedures, and then apply them.  The details of how to apply primitive
;; procedures are not the concern of the evaluator.  The evaluator just provides
;; the means of combination/abstraction that binds a collection of primitives
;; together to form a language.

;; The evaluator is used to evaluate nested expressions.  Simply applying a
;; primitive operation to primitive arguments works for (+ 1 2), but we need the
;; evaluator to "choreograph procedure composition" when we have an expression
;; like (+ 1 (* 2 3)).  We need to evaluate (* 2 3) before trying to evaluate
;; the addition.

;; The evaluator is used for defining and keeping track of variables,
;; procedures, and special forms.

;; We can write the evaluator independent of specific syntax - i.e. when we want
;; to know what type of expression we are evaluation, we'll use a function like
;; (assignment? exp) to check if an expression is performing an assignment.  We
;; can implement this method to check for the expression to look like (set! var
;; val), but we won't be specifically tied to the set! keyword.  We will also
;; define selectors (assignment-variable exp) and (assignment-value exp) to get
;; the data from the expressions.

;; The arguments to eval are an expression to evaluate and an environment to
;; evaluate it in.  It will classify the expression and figure out how to
;; proceed, so it's basically a big case statement.  Since we are using
;; functions like (assignment? exp) to check the type, we aren't tied to a
;; specific syntax. So we have an abstract syntax for our language that can
;; easily be changed by redefining functions like (assignment? exp).

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)                                 ;; numbers/strings are self-evaluating
        ((variable? exp)    (lookup-variable-value exp env))         ;; look up variable in current env
        ((quoted? exp)      (text-of-quotation exp))                 ;; return quoted expression
        ((assignment? exp)  (eval-assignment exp env))               ;; recurse to compute the new value, then modify the env to add/change variable binding
        ((definition? exp)  (eval-definition exp env))
        ((if? exp)          (eval-if exp env))                       ;; skips evaluation of either consequent or alternative, so it can't be treated like a normal procedure
        ((lambda? exp)      (make-procedure (lambda-parameters exp)  ;; defines a new applicable procedure (so it won't actually create a frame yet)
                                            (lambda-body exp)
                                            env))
        ((begin? exp)       (eval-sequence (begin-actions exp) env)) ;; execute all expressions in order
        ((cond? exp)        (eval (cond->if exp) env))               ;; transformed into nested if expressions and then evaluated
        ((application? exp) (apply (eval (operator exp) env)         ;; evaluate the operator/operands, and pass this off to apply
                                   (list-of-values (operands exp) env)))
        ((let? exp)         (eval (let->combination exp) env))       ;; implemented in Ex 4.6
        (else               (error "Unknown expression type in eval: " exp))))

;; The arguments to apply are an procedure and a list of arguments to apply the
;; procedure to.  It distinguishes between built-in primitive procedures and
;; compound procedures.  To apply a compound procedure, it must evaluate the
;; expressions that make up the body of the procedure.  The environment that
;; gets passed in to eval to do this is the base environment that the procedure
;; came from.

(define (apply proc args)
  (cond ((primitive-procedure? proc) (apply-primitive-procedure proc args))
        ((compound-procedure? proc)  (eval-sequence (procedure-body proc)
                                                    (extend-env (procedure-parameters proc)
                                                                args
                                                                (procedure-env proc))))
        (else                        (error "Unknown procedure type in apply: " proc))))

;; Now, implementing the interpreter is a matter of implementing these small,
;; simple functions.  This turns out to be particularly easy in Scheme since we
;; have the ability to quote data.  For example, we can extract the first symbol
;; from a list without having to actually parse the text itself.

;; list-of-values gives us a list of arguments to pass on to apply.  It evals
;; each operand in the original exp passed to eval:

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; eval-if has to choose which expressions to actually evaluate.  Again, we
;; don't want to tie ourselves to a specific syntax, so we even need an
;; abstraction for true/false.  This can help us recognize the distinction
;; between an implemented language and an implementation language.  We when
;; (eval (if-predicate exp) env), we are evaluating if-predicate in the language
;; being implemented, so it will yield a boolean in that language.  But we need
;; our interpreter to translate that into a boolean in the implementation
;; language.

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; eval-sequence is used to execute a series of expressions.  apply uses it to
;; evaluate code in function bodies, and eval uses it to execute code in begin
;; expressions.

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; We can define eval-assignment and eval-definition by passing them specific
;; environments where we want the value to be bound.  The tricky part is picking
;; which environment is the correct environment, but we'll deal with that later.
;; We return 'ok, but this is undefined in the Scheme standard:

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (assignment-variable exp)
                    (eval (assignment-value exp) env)
                    env)
  'ok)

;; syntax specification ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now we can start defining functions to identify the type of an expression,
;; and selector functions to extract parts of an expression.

;; Only numbers/strings are self-evaluating:

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;; Variables are symbols:

(define variable? symbol?)
;; For some reason, the book writes the definition as
;; (define (variable? exp) (symbol? exp))
;; I think these are the same, but I should check on that...

;; We use a function (tagged-list? exp tag) to check what the first symbol in a
;; list is.  We pass the tag as a quoted symbol to check against.

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; Using this, we can check if we have a quoted expression, because it has the form
;; (quote <text-of-quotation>):

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

;; Assignments start with set!:

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; Definitions have a few different forms:

;; (define <var> <value>)

;; (define (<var> <param-1> ... <param-n>)
;;   <body>)

;; (define <var>
;;   (lambda (<param-1> ... <param-n>)
;;     <body>))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp)) ;; if the second elt is a symbol, we have (define <var> <value>)
      (cadr exp)           ;; (car (cdr exp)) == (car (<var> <value>)) == <var>
      (caadr exp)))        ;; if the second elt is a list, we have (define (<var> <ps>) <body>)
                           ;; (car (car (cdr exp))) == (car (car ((<var> <ps>) <body>))) == (car (<var> <ps>)) == <var>

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)                ;; (car (cdr (cdr exp))) == (car (cdr (<var> <value>))) == (car (<value>)) == <value>
      (make-lambda (cdadr exp)   ;; (cdr (car (cdr exp))) == (cdr (car ((<var> <ps>) <body>))) == (cdr (<var> <ps>)) == <ps>
                   (cddr exp)))) ;; (cdr (cdr exp)) == (cdr ((<var> <ps>) <body>))) == <body>

;; We need to handle lambda's separately from named function definitions:

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda params body)
  (cons 'lambda (cons params body)))

;; Conditionals don't need to provide an alternative, so we'll just return
;; 'false.  This is unspecified in the Scheme standard.

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

;; make-if will be used to convert cond statements to if statements:

(define (make-if pred con alt)
  (list 'if pred con alt))

;; begin statements group a bunch of expressions together.  We'll implement
;; selectors to grab the list of expressions, the next (i.e. first) expression, and
;; the tail of expressions:

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;; In order to translate cond expressions to if expressions, we need to be able
;; to take a sequence of expressions and convert them to a single expression:

(define (sequence->exp seq)
  (cond ((null? seq)     seq)
        ((last-exp? seq) (first-exp seq))
        (else            (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;; All compound expressions that don't match one of the above types are
;; procedure applications:

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operand ops) (cdr ops))

;; We are going to transform cond expressions to if expressions:

;; (cond ((p1   c1)
;;        (p2   c2)
;;        (else c3)))

;; is the same as

;; (if p1
;;     c1
;;     (if p2
;;         c2
;;         c3))

;; The ability to transform code into other code turns out to be pretty
;; important.  We know that cond is a special form because it has special
;; evaluation rules.  But it is implemented in terms of the simpler special form
;; `if`.  If we transform cond before we evaluate it, we can just evaluate it
;; like an if statement.

;; This means we don't have to implement an evaluation for cond specifically -
;; it can just evaluate it like an if expression.  Our evaluator can operate on
;; a limited set of special forms, which makes it simpler.  This also gives us
;; the general ability to define derived expressions, which are implemented as
;; syntactic transformation rather than as special forms.

(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-cond-clauses (cond-clauses exp)))

(define (expand-cond-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "else clause must be last (in cond->if):" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-cond-clauses rest))))))

;; let is another example of a derived expression, because it can be implemented
;; using lambdas:

;; (let ((<var-1> <exp-1>) ... (<var-n> <exp-n>))
;;   <body>)

;; is the same as

;; ((lambda (<var-1> ... <var-n>)
;;    <body>)
;;  <exp-1>
;;  ...
;;  <exp-n>)

;; Ex 4.6)

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-clauses exp) (cadr exp))
(define (let-expressions exp)
  (map cadr (let-clauses exp)))

(define (let-body exp) (caddr exp))
(define (let-clause-variable clause) (car clause))
(define (let-clause-value clause) (cdr clause))

(define (let->combination exp)
  (make-lambda (let-clauses exp)
               (let-body exp)
               (let-args exp)))

(define (make-lambda clauses body args)
  (list '('lambda clauses body) args))

;; (and (= 1 2)
;;      (= 3 4))

;; Ex 4.4)

;; exp == (and pred-1 ... pred-n)
(define (and? exp)
  (tagged-list? exp 'and))

(define and-preds operands)

(define (eval-and exp env)
  (eval-and-preds (and-preds exp) env))

(define (eval-and-preds preds env)
  (cond ((null? preds)                   true)                               ;; no preds
        ((false? (eval (car preds) env)) false)                              ;; false pred-1
        ((null? (cdr preds))             (eval (car preds) env))             ;; only pred-1
        (else                            (eval-and-preds (cdr preds) env)))) ;; many preds

;; exp == (or pred-1 ... pred-n)
(define (or? exp)
  (tagged-list? exp 'or))

(define or-preds operands)

(define (eval-or exp env)
  (eval-or-preds (or-preds exp) env))

(define (eval-or-preds preds env)
  (cond ((null? preds)                     false)                  ;; no preds
        ((true? (eval (car preds) env))    (eval (car preds) env)) ;; true pred-1
        (else (eval-or-preds (cdr preds)   env))))                 ;; recur

;; Ex. 4.4 as derived expressions)

(define (and->if exp)
  (expand-and-preds (and-preds exp)))

(define (expand-and-preds preds)
  (if (null? preds)
      'false
      (let ((first (car preds))
            (rest (cdr preds)))
        (make-if (not first)
                 false
                 (expand-and-preds rest)))))


(define (or->if exp)
  (expand-or-preds (or-preds exp)))

(define (expand-or-preds preds)
  (if (null? preds)
      'true
      (let ((first (car preds))
            (rest (cdr preds)))
        (make-if first
                 true
                 (expand-or-preds rest)))))


;; evaluator data structures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The evaluator is also responsible for the representation of the internal data
;; structures the language uses, like procecures, environments, and boolean
;; values.

(define (true? x)
  (not (false? x)))

(define (false? x)
  (eq? x false))

;; We will assume we have these functions for now:

(define (apply-primitive-procedure proc args) '()) ;; TODO
(define (primitive-procedure? proc) '())           ;; TODO

;; Compound procedures are made from (make-procedure params body env):

(define (make-procedure params body env)
  (list 'procedure params body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-params p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-env p) (cadddr p))


;; A lot of the complexity of implementing an interpreter is handling different
;; environments, which will be represented as a list of frames.  The first frame
;; is (car env), and the base ("enclosing") environment is (cdr env):

(define (enclosing-env env) (cdr env))
(define (first-frame env) (car env))
(define empty-env '())

;; Each frame is a list of variable bindings, so we can represent it as a list
;; of variables and a list of values.  We will car the variable names onto the
;; values, so we'll end up with something like this:

;; ((x y z) 1 2 3)

(define (make-frame vars vals)
  (cons vars vals))

(define (frame-vars frame) (car frame))
(define (frame-vals frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;; When we execute procedures, we need to add a new frame to the environment:

(define (extend-env vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (error "must have same number of vars and vals")))

;; Variable lookup works like method lookup in object-oriented languages - it
;; starts at the current frame, and works its way up the chain until it finds a
;; binding for the variable:

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-env env)))
            ((eq? var (car vars))   (car vals))
            (else                   (scan (cdr vars) (cdr vals)))))
    (if (eq? env empty-env)
        (error "Couldn't find variable: " var)
        (let ((frame (first-frame env)))
          (scan (frame-vars frame)
                (frame-vals frame)))))
  (env-loop env))

;; We also have to traverse the chain of frames to try to set a variable:

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-env env)))
            ((eq? var (car vars))   (set-car! vals val))
            (else                   (scan (cdr vars) (cdr vals)))))
    (if (eq? env empty-env)
        (error "Couldn't find variable: " var)
        (let ((frame (first-frame env)))
          (scan (frame-vars frame)
                (frame-vals frame)))))
  (env-loop env))

;; When we define a variable, we check to see if it's already defined and
;; overwrite it.  If it's not define, add a new binding to the current
;; environment:

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)         (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else                 (scan (cdr vars) (cdr vals)))))
    (scan (frame-vars frame)
          (frame-vals frame))))

;; Ex. 4.11)
;; Now a frame will be a list of pairs: ((x 1) (y 2) (z 3))

;; (make-frame (x y z) (1 2 3))

(define (zip list1 list2)
  (define (iter xs ys zs)
    (if (null? xs)
        (reverse zs)
        (iter (cdr xs)
              (cdr ys)
              (cons (cons (car xs) (list (car ys)))
                    zs))))
  (if (not (eq? (length list1) (length list2)))
      (error "can't zip lists of different length")
      (iter list1 list2 '())))

(assert-eq '()
           (zip '() '()))

(assert-eq '((1 2))
           (zip '(1) '(2)))

(assert-eq '((1 4) (2 5) (3 6))
           (zip '(1 2 3) '(4 5 6)))

(define (make-frame vars vals)
  (zip vars vals))

(define (frame-vars frame)
  (map car frame))

(define (frame-vals frame)
  (map cadr frame))

(define (add-binding-to-frame! var val frame)
  (cons '(var val) frame))


;; The only thing left to do is give our interpreter a way to apply primitive
;; operations to primitive values.  We'll create a global environment by
;; extending the empty env.  It will hold bindings to primitive operations,
;; and primitive values like true/false:

(define (setup-env)
  (let ((initial-env (extend-env (primitive-procedure-names)
                                 (primitive-procedure-objects)
                                 empty-env)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define global-env (setup-env))

;; We will represent primitives with a `primitive` tag:

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

;; When we create the global env, it gets the bindings for primitives from these lists:

(define (primitive-procedures)
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null)
        ;; etc.
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;; To apply a primitive procedure, we simply apply the implementation procedure
;; to the arguments, using the underlying lisp system:

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc)
                              args))

;; We need to use apply-in-underlying-scheme, which is Scheme's built-in apply.
;; But since the metacircular evaluator needs to actually define/use an `apply`
;; method, and when we define it, it will shadow the definition of the built-in
;; `apply`.  We can get around this by aliasing the built-in apply to
;; apply-in-underlying-scheme:

(define apply-in-underlying-scheme apply)

;; Of course, we would have to do this before defining the `apply` method for
;; the evaluator.

;; Then we just need to write a repl:

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval output:")

(define (repl-cycle)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input global-env)))
      (announce-output output-prompt)
      (user-print output)))
  (repl-cycle))

(define (prompt-for-input str)
  (print "\n\n" str "\n"))

(define (prompt-for-input str)
  (print "\n" str "\n"))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(print "ok!\n\n\nsuccess!\n\n")
