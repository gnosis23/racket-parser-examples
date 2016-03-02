#lang racket
(require parser-tools/lex
         parser-tools/yacc)
(require (prefix-in : parser-tools/lex-sre))

;; define token-NUM, token-VAR
(define-tokens a (NUM VAR COMMENT))

(define-empty-tokens b (MINUS EOF LET IN COMMA OP CP = ZERO?
                              THEN ELSE IF))


(define-lex-abbrevs
  (digit10 (char-range "0" "9"))
  (number10 (:: (:? (:or "-" "+")) (:+ digit10)
                (:? (:: "." (:? (:+ digit10))))))
  (identifier-characters (:or (char-range "A" "z")
                              "?" "!" ":" "$" "^" "&"))
  (identifier (:+ identifier-characters))
  (comment (:: "%" (:* (char-complement #\newline))
               (:? #\newline))))

(define let-lang-lexer
  (lexer
   ["-" (token-MINUS)]
   ["let" (token-LET)]
   ["in" (token-IN)]
   ["," (token-COMMA)]
   ["(" (token-OP)]
   [")" (token-CP)]
   ["=" (token-=)]
   ["zero?" (token-ZERO?)]
   ["then" (token-THEN)]
   ["else" (token-ELSE)]
   ["if" (token-IF)]
   [(:+ number10) (token-NUM (string->number lexeme))]
   [identifier (token-VAR lexeme)]
   [whitespace (let-lang-lexer input-port)]
   [comment (let-lang-lexer input-port)]
   [(eof) (token-EOF)]))

(define-struct let-exp (var num exp) #:transparent)
(define-struct if-exp (exp b1 b2) #:transparent)
(define-struct diff-exp (e1 e2) #:transparent)
(define-struct const-exp (n) #:transparent)
(define-struct var-exp (i) #:transparent)
(define-struct zero?-exp (exp) #:transparent)

(define let-lang-parser
  (parser
   (start exp)
   (end EOF)
   (error (lambda (a b c)
            (printf "~a ~a ~a~%" a b c)
            void))
   (tokens a b)
   ; (precs (left MINUS))
   (grammar
    (exp ((LET VAR = exp IN exp)
          ; =>
          (let-exp $2 $4 $6))
         
         ((NUM) (const-exp $1))
         ((VAR) (var-exp $1))

         ((MINUS OP exp COMMA exp CP)
          ; =>
          (diff-exp $3 $5))

         ((ZERO? OP exp CP)
          ; =>
          (zero?-exp $3))

         ((IF exp THEN exp ELSE exp)
          ; =>
          (if-exp $2 $4 $6))))))

(define (lex-this lexer input)
  (lambda () (lexer input)))

(define (let-lang-ast str)
  (let-lang-parser (lex-this let-lang-lexer (open-input-string str))))


(define (value-of exp env)
  (match exp
    [(const-exp num) num]
    [(var-exp var) (cadr (assoc var env))]
    [(diff-exp exp1 exp2)
     (let ([val1 (value-of exp1 env)]
           [val2 (value-of exp2 env)])
       (- val1 val2))]
    [(zero?-exp exp1)
     (let ([val (value-of exp1 env)])
       (equal? 0 val))]
    [(if-exp exp1 b1 b2)
     (let ([val1 (value-of exp1 env)])
       (if val1
           (value-of b1 env)
           (value-of b2 env)))]
    [(let-exp var exp1 body)
     (let ([val1 (value-of exp1 env)])
       (value-of body (cons (list var val1) env)))]))

(define a1 (let-lang-ast "let a = -(100, 99) in if zero?(a) then 1 else -(a, 1)"))
;; (pretty-print a1)
(print (value-of a1 '()))
