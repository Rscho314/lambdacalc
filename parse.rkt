#lang racket

(require parser-tools/yacc
         "lex.rkt")
(provide parse-exec)

(define lc-parser
  (parser
   (start start)
   (end EOF)
   (tokens value-tokens delimiter-tokens)
   (error (lambda (tok-ok? tok-name tok-value)
            (display (list tok-ok? tok-name tok-value))))
   (grammar
    (start
     [(expression) $1]
     [() #f])
   (expression [(name) $1]
               [(function) $1]
               [(application) $1])
   (name [(NAME) `(name . ,(string->symbol $1))])
   (function [(LAMBDA NAME D expression) `(function ,(string->symbol $2) ,$4)])
   (application [(OP expression expression CP) `(application ,$2 ,$3)]))))

(define (parse parser lexer s)
(let ([input-port (open-input-string s)])
  (define (run)
    (port-count-lines! input-port)
    (parser (lambda () (lexer input-port))))
  (run)))

(define (parse-exec s)
  (parse lc-parser lc-lexer s))

#;(parse-exec "x")
#;(parse-exec "λx.x")
#;(parse-exec "(x x)")
#;(parse-exec "λf.λa.(f a)")
#;(parse-exec "(λx.x y)")