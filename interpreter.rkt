#lang racket

(require racket/include)
(include "lexer_parser.rkt")
(include "helpers.rkt")

(define (apply-env env search-var)
(cases environment env
	(empty-env ()
		(error "apply-env: Variable is not defined in environment")
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
				(else (error "value-of-command: Invalid Command"))
			)
		)
	)
)
)

(define (value-of-keyword kw env)
(cases keyword kw
	(if-keyword (my-if-stmnt) (value-of-if my-if-stmnt env))
	(assign-keyword (my-assign-stmnt) (value-of-assign my-assign-stmnt env))
	(while-keyword (my-while-stmnt) (value-of-while my-while-stmnt env))
	(ret-keyword (my-ret-stmnt) (value-of-return my-ret-stmnt env))
)
)

(define (value-of-if my-if-stmnt env)
(cases if-statement my-if-stmnt
	(if-stmnt (exp1 cmd1 cmd2)
		(let ([exp-value (expval->value (value-of-exp exp1 env))])
		(if exp-value
			(value-of-command cmd1 env)
			(value-of-command cmd2 env)
		)
		)
	)
)
)

(define (value-of-while my-while-stmnt env)
(cases while-statement my-while-stmnt
	(while-stmnt (exp1 cmd)
		(let ([exp-value (expval->value (value-of-exp exp1 env))])
			(if exp-value
				(let ([done-cmd (value-of-command cmd env)])
					(cases expval done-cmd
						(command-val (myexpval new-env has-returned) (if has-returned done-cmd (value-of-while my-while-stmnt new-env)))
						(else (error "value-of-while: Invalid Command"))
					)
				)
				(command-val (null-val) env #f)
			)
		)
	)
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
	(greater-exp (aexp1 aexp2) (bool-val (greater-expression? (expval->value (value-of-aexp aexp1 env)) (expval->value (value-of-aexp aexp2 env)))))
	(less-exp (aexp1 aexp2) (bool-val (less-expression? (expval->value (value-of-aexp aexp1 env)) (expval->value (value-of-aexp aexp2 env)))))
	(equal-exp (aexp1 aexp2) (bool-val (equal-expression? (expval->value (value-of-aexp aexp1 env)) (expval->value (value-of-aexp aexp2 env)))))
	(notequal-exp (aexp1 aexp2)
		(bool-val (not (equal-expression? (expval->value (value-of-aexp aexp1 env)) (expval->value (value-of-aexp aexp2 env))))))
)
)

(define (value-of-aexp myaexp env)
(cases aexp myaexp
	(b-exp (mybexp) (value-of-bexp mybexp env))
	(b-minus-a (bexp1 aexp1) (value->expval (subtract (expval->value (value-of-bexp bexp1 env)) (expval->value (value-of-aexp aexp1 env)))))
	(b-plus-a (bexp1 aexp1) (value->expval (add (expval->value (value-of-bexp bexp1 env)) (expval->value (value-of-aexp aexp1 env)))))
)
)

(define (value-of-bexp mybexp env)
(cases bexp mybexp
	(c-exp (mycexp) (value-of-cexp mycexp env))
	(c-times-b (cexp1 bexp1) (value->expval (mult (expval->value (value-of-cexp cexp1 env)) (expval->value (value-of-bexp bexp1 env)))))
	(c-dividedby-b (cexp1 bexp1) (value->expval (div (expval->value (value-of-cexp cexp1 env)) (expval->value (value-of-bexp bexp1 env)))))
)
)

(define (value-of-cexp mycexp env)
(cases cexp mycexp
	(minus-c-exp (cexp) (negate-expval (value-of-cexp cexp env)))
	(p-exp-p (myexp) (value-of-exp myexp))
	(num-exp (num) (num-val num))
	(null-exp () (null-val))
	(var-exp (var) (apply-env env var))
	(bool-exp (bool) (bool-val bool))
	(string-exp (str) (string-val str))
	(list-exp (mylist) (value-of-list mylist env))
	(var-list-exp (var my-list-member)
		(let ([list-expval (apply-env env var)])
		(cases expval list-expval
			(list-val (ls) (value-of-list-member ls my-list-member env))
			(else (error "value-of-cexp: Non-array object could not be subscripted"))
		)
		)
	)
)
)

(define (value-of-list ls env)
(cases list-t ls
	(empty-list () (list-val `()))
	(nonempty-list (values) (value-of-list-values values env))
)
)

(define (value-of-list-values values env)
(cases list-values values
	(single-list-value (myexp) (list-val (list (expval->value (value-of-exp myexp env)))))
	(nested-list-value (myexp other-values) 
		(list-val (cons (expval->value (value-of-exp myexp env)) (expval->list (value-of-list-values other-values env)))))
)
)

(define (value-of-list-member var mymember env)
(cases list-member mymember
	(single-list-member (myexp) (value->expval (get-subscript var (value-of-exp myexp env))))
	(nested-list-member (myexp other-members) (value-of-list-member (get-subscript var (value-of-exp myexp env)) other-members env))
)
)

(evaluate "code.txt")
