#lang racket

(define parse (lambda (statement)
                 (cond
                  ((symbol? statement) (list 'var-exp statement))
                  ((number? statement) (list 'num-exp statement))
                  ((and
                   (list? statement)
                   (eq? 'function (car statement))
                   (eq? (length statement) 3))(list 'func-exp (list (parse (car (cadr statement)))) (parse (caddr statement))))
                  ((and
                    (list? statement)
                    (eq? 'call (car statement))
                    (eq? (length statement) 3))(list 'app-exp (parse (cadr statement)) (parse (caddr statement))))
                  ((and
                    (list? statement)
                    (check_bool_op (car statement))
                    (eq? (length statement) 3)) (list 'bool-exp (car statement) (parse (cadr statement)) (parse (caddr statement))))
                  ((and
                    (list? statement)
                    (eq? '! (car statement))
                    (eq? (length statement) 2)) (list 'bool-exp (car statement) (parse (cadr statement))))
                  ((and
                    (list? statement)
                    (eq? 'ask (car statement))
                    (eq? (length statement) 4)) (list 'ask-exp (parse (cadr statement)) (parse (caddr statement)) (parse (cadddr statement))))
                  ((and
                    (list? statement)
                    (check_math_op (car statement))
                    (eq? (length statement) 3)) (list 'math-exp (car statement) (parse (cadr statement)) (parse (caddr statement))))
                  (else (print "Parse Fault: Invalid Statement"))
                  )
                 
                )
  )


(define check_bool_op (lambda (op)
                        (cond
                          ((eq? op '>) #t)
                          ((eq? op '<) #t)
                          ((eq? op '>=) #t)
                          ((eq? op '<=) #t)
                          ((eq? op '!=) #t)
                          ((eq? op '==) #t)
                          ((eq? op '&&) #t)
                          ((eq? op '||) #t)
                          ((eq? op '!) #t)
                          (else #f)
                          )
                        )
  )

(define check_math_op (lambda (op)
                        (if (eq? (list-ref '(+ - * / // %) op) -1) #f #t)
                        )
  )
                          
                          
                          

(provide (all-defined-out))