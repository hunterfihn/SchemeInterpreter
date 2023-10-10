#lang racket


(define resolve_env (lambda (varname env)
                      (cond
                        ((null? env) (print "Error: Variable not in scope"))
                        ((eq? #f (resolve_scope varname (car env))) (resolve_scope varname (cdr env)))
                        (else (resolve_scope varname (car env)))
                       )
                      )
  )

(define resolve_scope (lambda (varname scope)
                        (cond
                          ((null? scope) #f)
                          ((eq? varname (car (car scope))) (car (cdr (car scope))))
                          (else
                           (resolve_scope varname (cdr scope)))
                         )
                        )
  )


(define push_to_env (lambda (varName varVal env)
                      (cons (list(list varName varVal)) env)
                      )
  )


(provide (all-defined-out))