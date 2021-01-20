(require parser-tools/lex
  (prefix-in : parser-tools/lex-sre)
  (lib "eopl.ss" "eopl")
  parser-tools/yacc
  racket/include)

(include "datatypes.rkt")

(define main-lexer
  (lexer-src-pos
  ((:or "while" "do" "end" "if" "then" "else" "return") (string->symbol lexeme))
  ((:or "=" "<" ">" "==" "!=" "+" "-" "*" "/") (string->symbol lexeme))
  (";" 'semicolon)
  ("(" 'openpar)
  (")" 'closepar)
  ("[" 'openbracket)
  ("]" 'closebracket)
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
(define-empty-tokens sym-tok (semicolon openpar closepar openbracket closebracket comma))

(define (tok-to-sym tok-name)
  (case tok-name
    ['semicolon ";"]
    ['openpar "("]
    ['closepar ")"]
    ['openbracket "["]
    ['closebracket "]"]
    ['comma ","]
    [else (symbol->string tok-name)]))

(define main-parser
  (parser
   (start gcommand)
   (end EOF)
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (displayln (string-append "Parser Error: Unexpected " (tok-to-sym tok-name) " at line = "
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
     ((openbracket glistvalues closebracket) (nonempty-list $2))
     ((openbracket closebracket) (empty-list)))
    (glistvalues
     ((gexp) (single-list-value $1))
     ((gexp comma glistvalues) (nested-list-value $1 $3)))
    (glistmember
     ((openbracket gexp closebracket) (single-list-member $2))
     ((openbracket gexp closebracket glistmember) (nested-list-member $2 $4))))))


(define (scan-and-parse input-file)
  (let ([file-lexer (lambda () (main-lexer input-file))]) (main-parser file-lexer)))
