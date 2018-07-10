#lang racket

(require "parse.rkt")

(define (lc-eval expression)
  (let ([type (car expression)]
         [value (cdr expression)])
    (match type
      ['name
       value]
      ['function
       ;normal order
       (let ([function-name (car value)]
             [function-body (cdr value)])
         `(function
           ,function-name
           ,(car
             (map
              (λ (p)
                (cond
                  [(equal? function-name (cdr p))
                   `(bound . ,(cdr p))]
                  [else p]))
              function-body))))]
      ['application
       (let ([operator (car value)]
             [operand (cdr value)])
         ;TODO: replace operator bound values by operand
         value)]
      [else (error "AST node could not be evaluated: " value)])))

(define (eval-exec s)
  (lc-eval (parse-exec s)))

(eval-exec "x")
(eval-exec "λx.x")