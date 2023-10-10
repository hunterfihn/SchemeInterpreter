#lang racket

(require "utility.rkt")


(define process (lambda (parse env)
                  (cond
                    ((null? parse) (displayln "Error: Invalid Parse"))
                    ((eq? 'var-exp (car parse)) (process_var_exp parse env))
                    ((eq? 'app-exp (car parse)) (process_app_exp parse env))
                    ((eq? 'num-exp (car parse)) (process_num_exp parse env))
                    ((eq? 'bool-exp (car parse))(process_bool_exp parse env))
                    ((eq? 'ask-exp (car parse)) (process_ask_exp parse env))
                    ;((eq? 'math-exp (car parse)) (process_math_exp parse env))
                    (else #f)
                   )
                  )
  )



(define process_var_exp (lambda (parse env)
                          (resolve_env (cadr parse) env)
                          )
  )


(define process_bool_exp (lambda (parse env)
                           (cond
                             ((eq? '> (cadr parse)) (> (process (caddr parse) env) (process (cadddr parse) env)))
                             ((eq? '< (cadr parse)) (< (process (caddr parse) env) (process (cadddr parse) env)))
                             ((eq? '<= (cadr parse)) (<= (process (caddr parse) env) (process (cadddr parse) env)))
                             ((eq? '>= (cadr parse)) (>= (process (caddr parse) env) (process (cadddr parse) env)))
                             ((eq? '== (cadr parse)) (eq? (process (caddr parse) env) (process (cadddr parse) env)))
                             ((eq? '!= (cadr parse)) (not (eq? (process (caddr parse) env) (process (cadddr parse) env))))
                             ((eq? '&& (cadr parse)) (and (process (caddr parse) env) (process (cadddr parse) env)))
                             ((eq? '|| (cadr parse)) (or (process (caddr parse) env) (process (cadddr parse) env)))
                             ((eq? '! (cadr parse)) (not (process (caddr parse) env)))
                             (else (print "Error: Illegal Boolean Expression"))
                            )
  )
 )
  
(define process_ask_exp (lambda (parse env)
                          (if (process (cadr parse) env) (process (caddr parse) env) (process (cadddr parse) env))
                          )
  )

(define process_app_exp (lambda (parse env)
                          (let ((local_env (push_to_env (cadr (car (cadr (cadr parse)))) (process (caddr parse) env) env)
                                )
                            )
                          (process (caddr (cadr parse)) local_env)
                          )
  )
  )


(define process_num_exp (lambda (parse env)
                          (cadr parse)
                          )
  )


(provide (all-defined-out))