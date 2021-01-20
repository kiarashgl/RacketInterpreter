#lang racket

(require racket/include)
(include "lexer_parser.rkt")
(define-datatype expval expval?
	(null-val)
	(num-val (num number?))
	(bool-val (bool boolean?))
	(string-val (str string?))
	(list-val (ls list?))
	(command-val (expval expval?) (env environment?) (has-returned boolean?))
)

(define (expval->num exp)
(cases expval exp
	(num-val (num) num)
	(else (error "Cannot cast exp-val to num"))
))

(define (expval->bool exp)
(cases expval exp
	(bool-val (bool) bool)
	(else (error "Cannot cast exp-val to bool"))
))

(define (expval->string exp)
(cases expval exp
	(string-val (str) str)
	(else (error "Cannot cast exp-val to string"))
))

(define (expval->list exp)
(cases expval exp
	(list-val (ls) ls)
	(else (error "Cannot cast exp-val to list"))
))

(define (command-val->expval cmdval)
(cases expval cmdval
	(command-val (exp env has-returned) exp)
	(else (error "Cannot cast command-val to expval"))
))
(define-datatype environment environment?
	(empty-env)
	(extend-env
		(var symbol?)
		(val expval?)
		(env environment?)
	)
)

(define (apply-env env search-var)
(cases environment env
	(empty-env ()
		(error "Variable is not defined in environment")
	)
	(extend-env (saved-var saved-val saved-env)
		(if (eqv? saved-var search-var)
		saved-val
		(apply-env saved-env search-var))
	)
)
)

(define (evaluate input_file)
(let ([inp (open-input-file input_file)]) (begin
	(port-count-lines! inp)
	(command-val->expval (value-of-command (scan-and-parse inp) (empty-env)))
	)
)
)

(define (value-of-command cmd env)
(cases command cmd
	(keyword-only (keyword1) (value-of-keyword keyword1 env))
	(keyword-com (com1 keyword1)
	(let ([command-value (value-of-command com1 env)])
			(cases expval command-value
				(command-val (cmd-expval cmd-env has-returned)
					(if has-returned
						command-value
						(value-of-keyword keyword1 cmd-env)
					)
				)
				(else (error "Invalid Command"))
			)
		)
	)
)
)

(define (value-of-keyword kw env)
(cases keyword kw
	;(if-keyword (my-if-stmnt) (value-of-if my-if-stmnt env))
	(assign-keyword (my-assign-statement) (value-of-assign my-assign-statement env))
	;(while-keyword (my-while-statement) (value-of-while my-while-statement env))
	(ret-keyword (my-ret-statement) (value-of-return my-ret-statement env))
	(else (error "Invalid keyword"))

)
)

(define (value-of-assign assign env)
(cases assign-statement assign
	(assign-stmnt (myvar myexp)
		(let ([my-exp-value (value-of-exp myexp env)])
		(command-val my-exp-value (extend-env myvar my-exp-value env) #f)
		))
)
)

(define (value-of-return ret env)
(cases return-statement ret
	(return-stmnt (myexp) (command-val (value-of-exp myexp env) env #t))
)
)

(define (value-of-exp myexp env)
(cases exp myexp
	(a-exp (myaexp) (value-of-aexp myaexp env))
	(else (error "Invalid Exp"))
)
)

(define (value-of-aexp myaexp env)
(cases aexp myaexp
	(b-exp (mybexp) (value-of-bexp mybexp env))
	(else (error "Invalid Aexp"))
)
)

(define (value-of-bexp mybexp env)
(cases bexp mybexp
	(c-exp (mycexp) (value-of-cexp mycexp env))
	(else (error "Invalid Bexp"))
)
)

(define (value-of-cexp mycexp env)
(cases cexp mycexp
	(num-exp (num) (num-val num))
	(var-exp (var) (apply-env env var))
	(else (error "Invalid Cexp"))
)
)

(define equal-expression?
   (lambda (num1 num2)
     (cond
     [(and (null? num1) (null? num2)) #t]
     [(and (number? num1) (number? num2)) (equal? num1 num2)]
     [(and (string? num1) (string? num2)) (equal? num1 num2)]
     [(and (boolean? num1) (boolean? num2)) (eqv? num1 num2)]
     [(and (list? num1) (list? num2)) (equal? num2 num1)]
     [else #f] 
     )))
(evaluate "test0.txt")
