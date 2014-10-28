
(define apply-in-underlying-scheme apply) ;; we'll need the original apply later
(define (apply proc args)
  (cond ((primitive-procedure? proc) (apply-primitive-procedure proc args))
        ((compound-procedure? proc)  (eval-sequence (procedure-body proc)
                                                    (extend-env (procedure-params proc)
                                                                args
                                                                (procedure-env proc))))
        (else                        (error "Unknown procedure type in apply: " proc))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp)    (lookup-variable-value exp env))
        ((quoted? exp)      (text-of-quotation exp))
        ((assignment? exp)  (eval-assignment exp env))
        ((definition? exp)  (eval-definition exp env))
        ((if? exp)          (eval-if exp env))
        ((lambda? exp)      (make-procedure (lambda-parameters exp)
                                            (lambda-body exp)
                                            env))
        ((begin? exp)       (eval-sequence (begin-actions exp) env))
        ((cond? exp)        (eval (cond->if exp) env))
        ((application? exp) (apply (eval (operator exp) env)
                                   (list-of-values (operands exp) env)))
        ((let? exp)         (eval (let->combination exp) env))
        (else               (error "Unknown expression type in eval: " exp))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (cdr exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

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

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define variable? symbol?)
;; For some reason, the book writes the definition as
;; (define (variable? exp) (symbol? exp))
;; I think these are the same, but I should check on that...

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))



(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda params body)
  (cons 'lambda (cons params body)))

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))

(define (make-if pred con alt)
  (list 'if pred con alt))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq)     seq)
        ((last-exp? seq) (first-exp seq))
        (else            (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operand ops) (cdr ops))

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
      #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "else clause must be last (in cond->if):" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-cond-clauses rest))))))

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

(define (and? exp)
  (tagged-list? exp 'and))

(define and-preds operands)

(define (eval-and exp env)
  (eval-and-preds (and-preds exp) env))

(define (eval-and-preds preds env)
  (cond ((null? preds)                   #t)
        ((false? (eval (car preds) env)) #f)
        ((null? (cdr preds))             (eval (car preds) env))
        (else                            (eval-and-preds (cdr preds) env))))

(define (or? exp)
  (tagged-list? exp 'or))

(define or-preds operands)

(define (eval-or exp env)
  (eval-or-preds (or-preds exp) env))

(define (eval-or-preds preds env)
  (cond ((null? preds)                     #f)
        ((true? (eval (car preds) env))    (eval (car preds) env))
        (else (eval-or-preds (cdr preds)   env))))

;; ;; Ex. 4.4 as derived expressions)

;; (define (and->if exp)
;;   (expand-and-preds (and-preds exp)))

;; (define (expand-and-preds preds)
;;   (if (null? preds)
;;       'false
;;       (let ((first (car preds))
;;             (rest (cdr preds)))
;;         (make-if (not first)
;;                  false
;;                  (expand-and-preds rest)))))


;; (define (or->if exp)
;;   (expand-or-preds (or-preds exp)))

;; (define (expand-or-preds preds)
;;   (if (null? preds)
;;       'true
;;       (let ((first (car preds))
;;             (rest (cdr preds)))
;;         (make-if first
;;                  true
;;                  (expand-or-preds rest)))))


;; evaluator data structures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (true? x)
  (not (false? x)))

(define (false? x)
  (eq? x #f))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc)
                              args))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (make-procedure params body env)
  (list 'procedure params body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-params p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-env p) (cadddr p))

(define (enclosing-env env) (cdr env))
(define (first-frame env) (car env))
(define empty-env '())

(define (make-frame vars vals)
  (cons vars vals))

(define (frame-vars frame) (car frame))
(define (frame-vals frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-env vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (error "must have same number of vars and vals")))

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

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-env env)))
            ((eq? var (car vars))   (set-car! vals val))
            (else                   (scan (cdr vars) (cdr vals)))))
    (if (eq? env empty-env)
        (error "Couldn't find variable to set: " var)
        (let ((frame (first-frame env)))
          (scan (frame-vars frame)
                (frame-vals frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)         (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else                 (scan (cdr vars) (cdr vals)))))
    (scan (frame-vars frame)
          (frame-vals frame))))


;; set up session ;;
;;;;;;;;;;;;;;;;;;;;

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cddr cddr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '= =)
        (list '+ +)
        (list '* *)
        (list '- -)
        (list '/ /)
        (list 'print print)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-env)
  (let ((initial-env (extend-env (primitive-procedure-names)
                                 (primitive-procedure-objects)
                                 empty-env)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define global-env (setup-env))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc)
                              args))

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

(define (announce-output str)
  (print "\n" str "\n"))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-params object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(repl-cycle)
