#lang racket
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         syntax/readerr)

; Lexer abbreviations
(define-lex-abbrevs
  [character (:or (:/ "a" "z")(:/ #\A #\Z)  "?" "_")]
  [digit  (:/ #\0 #\9)]
  [space (:or #\tab #\space #\newline)]
  [operator (:or "+" "-" "~" "*" "/" "=" "!=" "<" ">" "<=" ">=" "&")]
  [reserved (:or "if" "then" "else" "let" "in" "map" "to"
               "empty" "true" "false" "number?" "function?" "list?" "empty?" "cons?" "cons" "first" "rest" "arity")])

; Tokens
(define-tokens cicom (ID INT OPERATOR))
(define-empty-tokens cicom* (newline EOF OP CP OB CB COMMA SEMI-COLON OR
                                     if then else let in map to empty
                                     ASSIGN + - ~ * / = != < > <= >= &
                                     number? function? list? empty? cons? cons first rest arity
                                     true false))

; Lexer
(define cicom-lexer
 (lexer

  [(eof) 'EOF]

  ; Skips spaces, tabs and newlines
  [(:or whitespace blank iso-control)
   (cicom-lexer input-port)]

  ; Matches left parenthesis
  [#\(
   (token-OP)]

  ; Matches right parenthesis
  [#\)
   (token-CP)]

  ; Matches left bracket
  [#\[
   (token-OB)]

  ; Matches right bracket
  [#\]
   (token-CB)]

  ; Matches comma
  [#\,
   (token-COMMA)]

  ; Matches semi-colon
  [#\;
   (token-SEMI-COLON)]

  ; Matches with specified symbol
  [":="
   (token-ASSIGN)]

  ; Matches with specified symbol
  ["|"
   (token-OR)]

  ; Matches any reserved word or operator
  [(:or reserved operator)
   (string->symbol lexeme)]

  ; Matches any numbers
  [numeric
   (token-INT (string->number lexeme))]

  ; Matches any character with repetition of characters or numbers
  [(:: character(:* (:or character digit)))
   (token-ID (string->symbol lexeme))]
  ))

; Parser
(define cicom-parser
  (parser
   
   (start Exp)
   (end EOF)
   (error
    (lambda (tok-ok? tok-name tok-val)
      (cond
        [(equal? tok-name token-EOF) void]
        [else (print "error in -> ")
              (print tok-name)
              (print ": ")
              (print tok-val)])))
   (tokens cicom cicom*)

   (precs
    (left - +))

   (grammar

    (Exp [(Term Binop Exp) "OK"]
         [(Term) "OK"]
         [(if Exp then Exp else Exp) "OK"]
         [(let Def in Exp) "OK"]
         [(map IdList to Exp) "OK"])

    (Term [(Unop Term) "OK"]
          [(Factor) "OK"]
          [(Factor OP ExpList CP) "OK"]
          [(empty) "OK"]
          [(INT) "OK"]
          [(Bool) "OK"])

    (Factor [(OP Exp CP) "OK"]
            [(Prim) "OK"]
            [(ID) "OK"])

    (ExpList [() "OK"]
             [ (PropExpList) "OK"])

    (PropExpList [ (Exp) "OK"]
                 [ (Exp COMMA PropExpList) "OK"])

    (IdList [() "OK"]
            [(PropIdList) "OK"])

    (PropIdList [(ID) "OK"]
                [(ID COMMA PropIdList) "OK"])

    (Def
     [(ID ASSIGN Exp SEMI-COLON) "OK"]
     [(ID ASSIGN Exp SEMI-COLON Def) "OK"])

    (Unop [(Sign) "OK"]
          [(~) "OK"])

    (Sign [(+) "OK"]
          [(-) "OK"])

    (Binop [(Sign) "OK"]
           [(*) "OK"]
           [(/) "OK"]
           [(=) "OK"]
           [(!=) "OK"]
           [(<) "OK"]
           [(>) "OK"]
           [(<=) "OK"]
           [(>=) "OK"]
           [(&) "OK"]
           [(OR) "OK"])
           
    (Prim [(number?) "OK"]
          [(function?) "OK"]
          [(list?) "OK"]
          [(empty?) "OK"]
          [(cons?) "OK"]
          [(cons) "OK"]
          [(first) "OK"]
          [(rest) "OK"]
          [(arity) "OK"])

    (Bool [(true) "OK"]
          [(false) "OK"]))))

(define file (file->string "Test"))
(define (lex-this lexer input) (lambda () (lexer input)))
(cicom-parser (lex-this cicom-lexer (open-input-string file)))

  