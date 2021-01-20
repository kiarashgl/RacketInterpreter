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

(define (value->expval val)
(cond
	[(number? val) (num-val val)]
	[(boolean? val) (bool-val val)]
	[(list? val) (list-val val)]
	[(string? val) (string-val val)]
	[(null? val) (null-val)]
	[else (error "Cannot cast to exp-val")]
)
)

(define (expval->value myexp)
(cases expval myexp
	(num-val (num) num)
	(bool-val (bool) bool)
	(string-val (str) str)
	(list-val (ls) ls)
	(null-val () null)
	(command-val (exp env has-returned) (expval->value exp))
)
)

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
	(equal-exp (aexp1 aexp2) (bool-val (equal-expression? (expval->value (value-of-aexp aexp1 env)) (expval->value (value-of-aexp aexp2 env)))))
	(else (error "Invalid Exp"))
)
)

(define (value-of-aexp myaexp env)
(cases aexp myaexp
	(b-exp (mybexp) (value-of-bexp mybexp env))
	(else (error "Invalid Aexp"))
)
)

(define (mult value1 value2)
(cond
	[(and (number? value1) (number? value2)) (* value1 value2)]
	[(and (number? value1) (list? value2))
		(cond
			[(null? value2) value2]
			[(not (number? (car value2))) (error "Could not multiply a non-numerical list by a number")]
			[else (cons (* value1 (car value2)) (mult value1 (cdr value2)))])]
	[(and (list? value1) (number? value2)) (mult value2 value1)]
	[(and (boolean? value1) (boolean? value2)) (and value1 value2)]
	[(and (boolean? value1) (list? value2))
		(cond
			[(null? value2) value2]
			[(not (boolean? (car value2))) (error "Could not and a non-boolean list by a boolean")]
			[else (cons (and value1 (car value2)) (mult value1 (cdr value2)))])]
	[(and (list? value1) (boolean? value2)) (mult value2 value1)]
	[else (error "Invalid operands in multiplication")]	
)
)

(define (div value1 value2)
(cond
	[(and (number? value1) (number? value2)) (/ value1 value2)]
	[(and (number? value1) (list? value2))
		(cond
			[(null? value2) value2]
			[(not (number? (car value2))) (error "Could not multiply a non-numerical list by a number")]
			[else (cons (/ value1 (car value2)) (div value1 (cdr value2)))])]
	[(and (list? value1) (number? value2)) (cond
			[(null? value1) value1]
			[(not (number? (car value1))) (error "Could not multiply a non-numerical list by a number")]
			[else (cons (/ (car value1) value2) (div (cdr value1) value2))])]
	[else (error "Invalid operands in division")]	
)
)

(define (value-of-bexp mybexp env)
(cases bexp mybexp
	(c-exp (mycexp) (value-of-cexp mycexp env))
	(c-times-b (cexp1 bexp1) (value->expval (mult (expval->value (value-of-cexp cexp1 env)) (expval->value (value-of-bexp bexp1 env)))))
	(c-dividedby-b (cexp1 bexp1) (value->expval (div (expval->value (value-of-cexp cexp1 env)) (expval->value (value-of-bexp bexp1 env)))))
)
)

(define (negate value)
(cond
	[(number? value) (- value)]
	[(boolean? value) (not value)]
	[(list? value) (if (null? value) value (cons (negate (car value)) (negate (cdr value))))]
	[else (error "Invalid negate operation")]
)
)

(define (negate-expval myexpval)
(cases expval myexpval
	(num-val (num) (num-val (negate num)))
	(bool-val (bool) (bool-val (negate bool)))
	(list-val (ls) (list-val (negate ls)))
	(else (error "Invalid negate operation"))
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
	(var-list-exp (var my-list-member) (value-of-list-member (expval->list (apply-env env var)) my-list-member env))
)
)

(define (equal-expression? num1 num2)
	(cond
	[(and (null? num1) (null? num2)) #t]
	[(and (number? num1) (number? num2)) (equal? num1 num2)]
	[(and (string? num1) (string? num2)) (equal? num1 num2)]
	[(and (boolean? num1) (boolean? num2)) (eqv? num1 num2)]
	[(and (list? num1) (list? num2)) (equal? num2 num1)]
	[else #f] 
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

(define (get-subscript ls indexp)
(if (not (list? ls)) (error "Non-array object could not be subscripted")
(let ([index-number (expval->value indexp)])
	(if
		(not (number? index-number)) (error "Index should be a number")
		(get-list-index ls index-number)
	)
)
)
)

(define (value-of-list-member var mymember env)
(cases list-member mymember
	(single-list-member (myexp) (value->expval (get-subscript var (value-of-exp myexp env))))
	(nested-list-member (myexp other-members) (value-of-list-member (get-subscript var (value-of-exp myexp env)) other-members env))
)
)

(define (get-list-index ls ind)
(cond
	[(null? ls) (error "Index is out of range")]
	[(< ind 0) (error "Subscription index should not be negative.")]
	[(= ind 0) (car ls)]
	[else (get-list-index (cdr ls) (- ind 1))]
)
)
(define (compare-list-number? lst num)
  (if (empty? lst) #t                                                              
                (let ([carr (car lst)][cdrr (cdr lst)])
                   (and (if (list? carr)  (error (string-append "Cannot compare " (get-Type (element-to-expval carr)) " with " (get-Type (element-to-expval num))))
                      (greater-expression? carr num))
                        (greater-expression? cdrr num))
                         )))

  
(define greater-expression?
  (lambda (num1 num2)
   (cond
     [(and (number? num1) (number? num2)) (>  num1 num2)]
     [(and (string? num1) (string? num2)) (string>? num1 num2)]
     [(and (list? num1) (or (number? num2) (string? num2)))  (compare-list-number? num1 num2)]
     [(and (list? num2) (or (number? num1) (string? num1))) (compare-list-number? num2 num1)]
     [(error (string-append "Cannot compare " (get-Type (element-to-expval num1)) " with " (get-Type (element-to-expval num2))))]
     )))
     
     
(evaluate "test0.txt")
