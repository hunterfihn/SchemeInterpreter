#lang racket


(define resolve_env (lambda (varname env)
                      (cond
                        ((null? env) (print "Error: Variable not in scope"))
                        ((eq? #f (resolve_scope varname (car env))) (resolve_env varname (cdr env)))
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
                      (cons (list (list varName varVal)) env)
                      )
  )

(define is_in_list (lambda (lst item)
                     (cond
                       ((null? lst) #f)
                       ((eq? (car lst) item) #t)
                       (else (is_in_list (cdr lst) item))
                       )
                     )
  )


(define push_vars_to_env (lambda (list_vars list_vals env)
                           (cons (pair_helper list_vars list_vals) env)
                            )
  )

(define pair_helper (lambda (list_vars list_vals)
                      (if (null? list_vars) '()
                          (cons (list (car list_vars) (car list_vals)) (pair_helper (cdr list_vars) (cdr list_vals)))
                          )
                      )
  )

(define trim_to_global_scope
  (lambda (env)
    (cond
      ((not (pair? env)) (error-output "Illegal environment passed into the trim_to_global_scope argument list"))
      ((eq? 1 (length env)) env)
      (else (trim_to_global_scope (cdr env)))
      )
    )
  )

(define error-output (lambda (output) (displayln (string-append "***Error***: " output))
                      )
  )

(define pick_first_non_void (lambda (lst)
                              (cond
                                ((not (pair? lst)) (error-output "Received invalid parameter"))
                                ((and (pair? lst) (eq? (length lst) 1) (void? (car lst))) (display "***End***"))
                                ((void? (car lst)) (pick_first_non_void (cdr lst)))
                                (else (car list))
                                )
                              )
  )

(provide (all-defined-out))