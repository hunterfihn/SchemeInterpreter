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


(parse '(call (function (x) x) a))

(execute '(call (function (x) x) 1))