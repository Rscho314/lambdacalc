#lang racket

(require "parse.rkt")

(define (lc-eval expression)
  (if (pair? expression)
      (cond
        [(eq? (car expression) 'λ) expression]
        [(eq? (car expression) 'lc-apply)
         (lc-apply (cadr expression) (caddr expression))]
        [else expression])
      expression))

(define (lc-apply operator operand)
  ;normal order
  (let ([bound (cadr operator)]
        [body (caddr operator)])
    (caar
     (map (λ (e) (if (equal? e (car bound))
                     operand
                     e))
          body))))

(define (eval-exec s)
  (let ([parsed (parse-exec s)])
  (lc-eval parsed)))

(eval-exec "x")
(eval-exec "λx.x")
(eval-exec "(λx.x y)")
(eval-exec "λx.(x x)")