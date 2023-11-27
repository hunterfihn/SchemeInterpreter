#lang racket
(require "utility.rkt")
(require "parse.rkt")
(require "process.rkt")

(define var_env
  '(((a 1) (b 2) (c 3)))
  )

 
(define code '(each (a 0) (< a 5) (a (+ a 1)) ((out a))))



(define parsed (parse code))


(process parsed var_env)