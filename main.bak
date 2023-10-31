#lang racket
(require "utility.rkt")
(require "parse.rkt")
(require "process.rkt")

(define var_env
  '(((a 1) (b 2) (b 3)))
  )

 


(define execute (lambda (code)
                   (process (parse code) var_env)
                   )
  )

(define parsed '(call (function(x)(ask (== a 1) (+ x 1) (- x 1))) (2)))


; (call (functino (x y) (* x y)) (5 c))
; parsed -> (app-exp (func-exp ((var-exp x) (var-exp y)) (math-exp * (var-exp x) (var-exp y)))
;((num-exp 5) (var-exp c)))

(process (parse parsed) var_env)