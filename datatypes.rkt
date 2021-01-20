(define-datatype expval expval?
	(null-val)
	(num-val (num number?))
	(bool-val (bool boolean?))
	(string-val (str string?))
	(list-val (ls list?))
	(command-val (expval expval?) (env environment?) (has-returned boolean?))
)

(define-datatype environment environment?
	(empty-env)
	(extend-env
		(var symbol?)
		(val expval?)
		(env environment?)
	)
)

(define-datatype command command?
  (keyword-only (keyword1 keyword?))
  (keyword-com (com1 command?) (keyword1 keyword?)))

(define-datatype keyword keyword?
  (if-keyword (if-stmnt1 if-statement?))
  (assign-keyword (assign-stmnt1 assign-statement?))
  (while-keyword (while-stmnt1 while-statement?))
  (ret-keyword (return-stmnt1 return-statement?)))

(define-datatype while-statement while-statement?
  (while-stmnt (exp1 exp?) (com1 command?)))

(define-datatype if-statement if-statement?
  (if-stmnt (exp1 exp?) (com1 command?) (com2 command?)))

(define-datatype assign-statement assign-statement?
  (assign-stmnt (var1 symbol?) (exp1 exp?)))

(define-datatype return-statement return-statement?
  (return-stmnt (exp1 exp?)))

(define-datatype exp exp?
  (a-exp (aexp1 aexp?))
  (greater-exp (aexp1 aexp?) (aexp2 aexp?))
  (less-exp (aexp1 aexp?) (aexp2 aexp?))
  (equal-exp (aexp1 aexp?) (aexp2 aexp?))
  (notequal-exp (aexp1 aexp?) (aexp2 aexp?)))

(define-datatype aexp aexp?
  (b-exp (bexp1 bexp?))
  (b-minus-a (bexp1 bexp?) (aexp1 aexp?))
  (b-plus-a (bexp1 bexp?) (aexp1 aexp?)))

(define-datatype bexp bexp?
  (c-exp (cexp1 cexp?))
  (c-times-b (cexp1 cexp?) (bexp1 bexp?))
  (c-dividedby-b (cexp1 cexp?) (bexp1 bexp?)))

(define-datatype cexp cexp?
  (minus-c-exp (cexp1 cexp?))
  (p-exp-p (exp1 exp?))
  (num-exp (num1 number?))
  (null-exp)
  (var-exp (var1 symbol?))
  (bool-exp (bool1 boolean?))
  (string-exp (str1 string?))
  (list-exp (list-t1 list-t?))
  (var-list-exp (var1 symbol?) (list-member1 list-member?)))

(define-datatype list-t list-t?
  (empty-list)
  (nonempty-list (list-values1 list-values?)))

(define-datatype list-values list-values?
  (single-list-value (exp1 exp?))
  (nested-list-value (exp1 exp?) (list-values1 list-values?)))

(define-datatype list-member list-member?
  (single-list-member (exp1 exp?))
  (nested-list-member (exp1 exp?) (list-member1 list-member?)))
