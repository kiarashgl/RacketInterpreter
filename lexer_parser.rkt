(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         (lib "eopl.ss" "eopl")
         parser-tools/yacc)

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

(define main-lexer
           (lexer-src-pos
            ((:or "while" "do" "end" "if" "then" "else" "return") (string->symbol lexeme))
            ((:or "=" "<" ">" "==" "!=" "+" "-" "*" "/") (string->symbol lexeme))
            (";" 'semicolon)
            ("(" 'openpar)
            (")" 'closepar)
            ("[" 'openbra)
            ("]" 'closebra)
            ("," 'comma)
            ("true" (token-BOOL #t))
            ("false" (token-BOOL #f))
            ("null" (token-NULL))
            ((:or (:+ numeric) (:: (:+ numeric) #\. (:+ numeric))) (token-NUM (string->number lexeme)))
            ((:+ alphabetic) (token-VARIABLE (string->symbol lexeme)))
            ((:: #\" (:* (:~ #\")) #\") (token-STR (substring lexeme 1 (- (string-length lexeme) 1))))
            (whitespace (return-without-pos (main-lexer input-port)))
            ((eof) (token-EOF))))

(define-tokens val-tok (BOOL NUM STR VARIABLE))
(define-empty-tokens other-tok (NULL EOF))
(define-empty-tokens keyword-tok (while do end if then else endif return))
(define-empty-tokens op-tok (= < > == != + - * /))
(define-empty-tokens sym-tok (semicolon openpar closepar openbra closebra comma))

(define tok-to-sym (lambda (tok-name)
  (case tok-name
    ['semicolon ";"]
    ['openpar "("]
    ['closepar ")"]
    ['openbra "["]
    ['closebra "]"]
    ['comma ","]
    [else (symbol->string tok-name)])))


(define main-parser
  (parser
   (start gcommand)
   (end EOF)
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (displayln (string-append "ERROR unexpected " (tok-to-sym tok-name) " at line = "
                                    (number->string (position-line start-pos)) " column = "
                                    (number->string (+ 1 (position-col start-pos)))))))
   (src-pos)
   (tokens val-tok other-tok keyword-tok op-tok sym-tok)
   (grammar
    (gcommand ((gkeyword) (keyword-only $1)) ((gcommand semicolon gkeyword) (keyword-com $1 $3)))
    (gkeyword ((gif-stmnt) (if-keyword $1)) ((gassign-stmnt) (assign-keyword $1))
              ((gwhile-stmnt) (while-keyword $1)) ((greturn-stmnt) (ret-keyword $1)))
    (gwhile-stmnt ((while gexp do gcommand end) (while-stmnt $2 $4)))
    (gif-stmnt ((if gexp then gcommand else gcommand end) (if-stmnt $2 $4 $6)))
    (gassign-stmnt ((VARIABLE = gexp) (assign-stmnt $1 $3)))
    (greturn-stmnt ((return gexp) (return-stmnt $2)))
    (gexp
     ((gaexp) (a-exp $1)) ((gaexp > gaexp) (greater-exp $1 $3))
     ((gaexp < gaexp) (less-exp $1 $3)) ((gaexp == gaexp) (equal-exp $1 $3))
     ((gaexp != gaexp) (notequal-exp $1 $3)))
    (gaexp
     ((gbexp) (b-exp $1)) ((gbexp - gaexp) (b-minus-a $1 $3))
     ((gbexp + gaexp) (b-plus-a $1 $3)))
    (gbexp
     ((gcexp) (c-exp $1)) ((gcexp * gbexp) (c-times-b $1 $3))
     ((gcexp / gbexp) (c-dividedby-b $1 $3)))
    (gcexp
     ((- gcexp) (minus-c-exp $2)) ((openpar gexp closepar) (p-exp-p $2))
     ((NUM) (num-exp $1)) ((NULL) (null-exp)) ((VARIABLE) (var-exp $1))
     ((BOOL) (bool-exp $1)) ((STR) (string-exp $1)) ((glist) (list-exp $1))
     ((VARIABLE glistmember) (var-list-exp $1 $2)))
    (glist
     ((openbra glistvalues closebra) (nonempty-list $2))
     ((openbra closebra) (empty-list)))
    (glistvalues
     ((gexp) (single-list-value $1))
     ((gexp comma glistvalues) (nested-list-value $1 $3)))
    (glistmember
     ((openbra gexp closebra) (single-list-member $2))
     ((openbra gexp closebra glistmember) (nested-list-member $2 $4))))))


(define scan-and-parse (lambda (input-file)
  (let ([file-lexer (lambda () (main-lexer input-file))]) (main-parser file-lexer))))
