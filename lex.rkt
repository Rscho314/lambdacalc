#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide lc-lexer
         value-tokens
         delimiter-tokens)

(define-tokens value-tokens
  (NAME))

(define-empty-tokens delimiter-tokens
  (LAMBDA ;λ
   OP CP  ;( )
   D      ;.
   EOF))

(define lc-lexer
  (lexer
   [(eof) (token-EOF)]
   [(:or #\λ "lambda") (token-LAMBDA)]
   [(:+ (:/ #\a #\z)) (token-NAME lexeme)]
   [#\( (token-OP)]
   [#\) (token-CP)]
   [#\. (token-D)]
   [whitespace (lc-lexer input-port)]))
