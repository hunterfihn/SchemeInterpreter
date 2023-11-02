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

;(define code '(while (< a 5) (block (out a) (let ((a (+ a 1)))))))

(define code '(block (out a) (out b)))

(define parsed (parse code))


(process parsed var_env)