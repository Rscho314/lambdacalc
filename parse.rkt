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
   (name [(NAME) (string->symbol $1)])
   (function [(LAMBDA NAME D body) `(λ (,(string->symbol $2)) (,$4))])
   (body [(expression) $1])
   (application [(OP operator operand CP)
                 `(lc-apply ,$2 ,$3)])
   (operator [(expression) $1])
   (operand [(expression) (list $1)]))))

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
#;(parse-exec "λx.(x x)")