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
                    ((eq? 'math-exp (car parse)) (process_math_exp parse env))
                    ((eq? 'let-exp (car parse)) (process_let_exp parse env))
                    ((eq? 'when-exp (car parse)) ("test"))
                    ((eq? 'output-exp (car parse)) (displayln (string-append "***output***: "(number->string (process (cadr parse) env)))))
                    ((eq? 'block-exp (car parse)) (pick_first_non_void (map (lambda (code) (process code env)) (cdr parse))))
                    (else (error-output "Processor failed to handle parsed input"))
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

(define process_math_exp (lambda (parse env)
                           (cond
                             ((eq? '+ (cadr parse)) (+ (process (caddr parse) env) (process (cadddr parse) env)))
                             ((eq? '- (cadr parse)) (- (process (caddr parse) env) (process (cadddr parse) env)))
                             ((eq? '* (cadr parse)) (* (process (caddr parse) env) (process (cadddr parse) env)))
                             ((eq? '/ (cadr parse)) (quotient (process (caddr parse) env) (process (cadddr parse) env)))
                             ((eq? '// (cadr parse)) (/ (process (caddr parse) env) (process (cadddr parse) env)))
                             ((eq? '% (cadr parse)) (modulo (process (caddr parse) env) (process (cadddr parse) env)))
                             (else (print "Error: Illegal Math Expression"))
                           )
  )
  )

  
(define process_ask_exp (lambda (parse env)
                          (if (process (cadr parse) env) (process (caddr parse) env) (process (cadddr parse) env))
                          )
  )

(define process_app_exp (lambda (parse env)
    (let*
        (
         (global_env (trim_to_global_scope env))
         (local_env
          (push_vars_to_env
           (map (lambda (arg) (cadr arg)) (cdr (car (cadr (cadr parse)))))
           (map (lambda (val-exp) (process val-exp env))
                (cdr (caddr parse)))
           global_env)
          )
         )
      (process (caddr (cadr parse)) local_env)
      )
    )
  )
                          

(define add_to_top_scope (lambda (varname value env)
                           (cons
                            (cons
                            (list varname value) (car env)
                            )
                           (cdr env))
                         )
  )

(define process_let_exp (lambda (parse env)
                          (let*
                              ((varname_value_list
                                (map (lambda (pair)
                                       (list (cadr (car pair)) (process (cadr pair) env)))
                                     (cdr (cadr parse))))
                               (let_local_env (cons (append varname_value_list (car env)) (cdr env))))
                           (process (caddr parse) let_local_env)         
                           )
                          )
  )


(define process_num_exp (lambda (parse env)
                          (cadr parse)
                          )
  )


(provide (all-defined-out))