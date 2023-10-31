#lang racket
(require "utility.rkt")
(require "parse.rkt")
(require "process.rkt")

(define var_env
  '(((a 1) (b 3) (c 5)))
  )

 


(define execute (lambda (code)
                   (process (parse code) var_env)
                   )
  )

(define code '(call (function(x)(let ((d 10) (f 20)) (+ d (+ f x)))) (5)))

(define parsed (parse code))

; (call (functino (x y) (* x y)) (5 c))
; parsed -> (app-exp (func-exp ((var-exp x) (var-exp y)) (math-exp * (var-exp x) (var-exp y)))
;((num-exp 5) (var-exp c)))

(process parsed var_env)