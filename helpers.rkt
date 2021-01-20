(define (expval->num exp)
(cases expval exp
	(num-val (num) num)
	(else (error "expval->num: Cannot cast exp-val to num"))
))

(define (expval->bool exp)
(cases expval exp
	(bool-val (bool) bool)
	(else (error "expval->bool: Cannot cast exp-val to bool"))
))

(define (expval->string exp)
(cases expval exp
	(string-val (str) str)
	(else (error "expval->string: Cannot cast exp-val to string"))
))

(define (expval->list exp)
(cases expval exp
	(list-val (ls) ls)
	(else (error "expval->list: Cannot cast exp-val to list"))
))

(define (value->expval val)
(cond
	[(number? val) (num-val val)]
	[(boolean? val) (bool-val val)]
	[(list? val) (list-val val)]
	[(string? val) (string-val val)]
	[(null? val) (null-val)]
	[else (error "value->expval: Cannot cast to exp-val")]
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
	(else (error "command-val->expval: Cannot cast command-val to expval"))
))

(define (greater-expression? value1 value2)
	(cond
		[(and (number? value1) (number? value2)) (> value1 value2)]
		[(and (number? value1) (list? value2)) 
			(cond
				[(null? value2) #t]
				[(not (number? (car value2))) (error "greater-expression?: Could not compare number with non-numerical list")]
				[else (and (> value1 (car value2)) (greater-expression? value1 (cdr value2)))]
			)]
		[(and (list? value1) (number? value2)) 
			(cond
				[(null? value1) #t]
				[(not (number? (car value1))) (error "greater-expression?: Could not compare number with non-numerical list")]
				[else (and (> (car value1) value2) (greater-expression? (cdr value1) value2))]
			)]
		[(and (string? value1) (string? value2)) (string>? value1 value2)]
		[(and (string? value1) (list? value2)) 
			(cond
				[(null? value2) #t]
				[(not (string? (car value2))) (error "greater-expression?: Could not compare string with non-string list")]
				[else (and (string>? value1 (car value2)) (greater-expression? value1 (cdr value2)))]
			)]
		[(and (list? value1) (string? value2)) 
			(cond
				[(null? value1) #t]
				[(not (string? (car value1))) (error "greater-expression?: Could not compare string with non-string list")]
				[else (and (string>? (car value1) value2) (greater-expression? (cdr value1) value2))]
			)]
		[else (error "greater-expression?: Invalid operands in comparison")]
	)
)

(define (less-expression? value1 value2)
	(cond
		[(and (number? value1) (number? value2)) (< value1 value2)]
		[(and (number? value1) (list? value2)) 
			(cond
				[(null? value2) #t]
				[(not (number? (car value2))) (error "less-expression?: Could not compare number with non-numerical list")]
				[else (and (< value1 (car value2)) (less-expression? value1 (cdr value2)))]
			)]
		[(and (list? value1) (number? value2)) 
			(cond
				[(null? value1) #t]
				[(not (number? (car value1))) (error "less-expression?: Could not compare number with non-numerical list")]
				[else (and (< (car value1) value2) (less-expression? (cdr value1) value2))]
			)]
		[(and (string? value1) (string? value2)) (string<? value1 value2)]
		[(and (string? value1) (list? value2)) 
			(cond
				[(null? value2) #t]
				[(not (string? (car value2))) (error "less-expression?: Could not compare string with non-string list")]
				[else (and (string<? value1 (car value2)) (less-expression? value1 (cdr value2)))]
			)]
		[(and (list? value1) (string? value2)) 
			(cond
				[(null? value1) #t]
				[(not (string? (car value1))) (error "less-expression?: Could not compare string with non-string list")]
				[else (and (string<? (car value1) value2) (less-expression? (cdr value1) value2))]
			)]
		[else (error "less-expression?: Invalid operands in comparison")]
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

(define (subtract value1 value2)
(cond
	[(and (number? value1) (number? value2)) (- value1 value2)]
	[(and (number? value1) (list? value2))
		(cond
			[(null? value2) value2]
			[(not (number? (car value2))) (error "subtract: Could not subtract a non-numerical list by a number")]
			[else (cons (- value1 (car value2)) (subtract value1 (cdr value2)))])]

	[(and (list? value1) (number? value2))
		(cond
			[(null? value1) value1]
			[(not (number? (car value1))) (error "subtract: Could not subtract a non-numerical list by a number")]
			[else (cons (- (car value1) value2) (subtract (cdr value1) value2))])]
	[else (error "subtract: Invalid operands in subtraction")]	
)
)

(define (add value1 value2)
(cond
	[(and (number? value1) (number? value2)) (+ value1 value2)]
	[(and (number? value1) (list? value2))
		(cond
			[(null? value2) value2]
			[(not (number? (car value2))) (error "add: Could not add a non-numerical list by a number")]
			[else (cons (+ value1 (car value2)) (add value1 (cdr value2)))])]
	[(and (list? value1) (number? value2)) (add value2 value1)]
	[(and (boolean? value1) (boolean? value2)) (or value1 value2)]
	[(and (boolean? value1) (list? value2))
		(cond
			[(null? value2) value2]
			[(not (boolean? (car value2))) (error "add: Could not or a non-boolean list by a boolean")]
			[else (cons (or value1 (car value2)) (add value1 (cdr value2)))])]
	[(and (list? value1) (boolean? value2)) (add value2 value1)]
	[(and (list? value1) (list? value2)) (append value1 value2)]
	[(and (string? value1) (string? value2)) (string-append value1 value2)]

	[(and (string? value1) (list? value2))
		(cond
			[(null? value2) value2]
			[(not (string? (car value2))) (error "add: Could not append a string to a non-string list")]
			[else (cons (string-append value1 (car value2)) (add value1 (cdr value2)))])]
	[(and (list? value1) (string? value2))
		(cond
			[(null? value1) value1]
			[(not (string? (car value1))) (error "add: Could not append a string to a non-string list")]
			[else (cons (string-append (car value1) value2) (add (cdr value1) value2))])]
	[else (error "add: Invalid operands in addition")]	
)
)

(define (mult value1 value2)
(cond
	[(and (number? value1) (number? value2)) (* value1 value2)]
	[(and (number? value1) (list? value2))
		(cond
			[(null? value2) value2]
			[(not (number? (car value2))) (error "mult: Could not multiply a non-numerical list by a number")]
			[else (cons (* value1 (car value2)) (mult value1 (cdr value2)))])]
	[(and (list? value1) (number? value2)) (mult value2 value1)]
	[(and (boolean? value1) (boolean? value2)) (and value1 value2)]
	[(and (boolean? value1) (list? value2))
		(cond
			[(null? value2) value2]
			[(not (boolean? (car value2))) (error "mult: Could not and a non-boolean list by a boolean")]
			[else (cons (and value1 (car value2)) (mult value1 (cdr value2)))])]
	[(and (list? value1) (boolean? value2)) (mult value2 value1)]
	[else (error "mult: Invalid operands in multiplication")]	
)
)

(define (div value1 value2)
(cond
	[(and (number? value1) (number? value2)) (/ value1 value2)]
	[(and (number? value1) (list? value2))
		(cond
			[(null? value2) value2]
			[(not (number? (car value2))) (error "div: Could not multiply a non-numerical list by a number")]
			[else (cons (/ value1 (car value2)) (div value1 (cdr value2)))])]
	[(and (list? value1) (number? value2)) (cond
			[(null? value1) value1]
			[(not (number? (car value1))) (error "div: Could not multiply a non-numerical list by a number")]
			[else (cons (/ (car value1) value2) (div (cdr value1) value2))])]
	[else (error "div: Invalid operands in division")]	
)
)

(define (negate value)
(cond
	[(number? value) (- value)]
	[(boolean? value) (not value)]
	[(list? value) (if (null? value) value (cons (negate (car value)) (negate (cdr value))))]
	[else (error "negate: Invalid negate operation")]
)
)

(define (negate-expval myexpval)
(cases expval myexpval
	(num-val (num) (num-val (negate num)))
	(bool-val (bool) (bool-val (negate bool)))
	(list-val (ls) (list-val (negate ls)))
	(else (error "negate-expval: Invalid negate operation"))
)
)

(define (get-subscript ls indexp)
(if (not (list? ls)) (error "get-subscript: Non-array object could not be subscripted")
(let ([index-number (expval->value indexp)])
	(if
		(not (number? index-number)) (error "get-subscript: Index should be a number")
		(get-list-index ls index-number)
	)
)
)
)

(define (get-list-index ls ind)
(cond
	[(null? ls) (error "get-list-index: Index is out of range")]
	[(< ind 0) (error "get-list-index: Subscription index should not be negative.")]
	[(= ind 0) (car ls)]
	[else (get-list-index (cdr ls) (- ind 1))]
)
)
