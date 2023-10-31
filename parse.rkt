#lang racket
(require "utility.rkt")

(define parse (lambda (statement)
                 (cond
                  ((symbol? statement) (list 'var-exp statement))
                  ((number? statement) (list 'num-exp statement))
                  ((and
                   (list? statement)
                   (eq? 'function (car statement))
                   (eq? (length statement) 3))(list 'func-exp (list (parse (cadr statement))) (parse (caddr statement))))
                  ((and
                    (list? statement)
                    (eq? 'call (car statement))
                    (eq? (length statement) 3))(if (eq? (length (cadr (cadr statement))) (length (caddr statement))) (list 'app-exp (parse (cadr statement)) (parse (caddr statement))) (print "Error: Argument Mismatch")))
                  ((and
                    (list? statement)
                    (check_bool_op (car statement))
                    (eq? (length statement) 3))(list 'bool-exp (car statement) (parse (cadr statement)) (parse (caddr statement))))
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
                  ((and
                    (list? statement)
                    (eq? 'let (car statement))
                    (eq? (length statement) 3)) (list 'let-exp (cons 'list-exp (map (lambda (pair) (map (lambda (item) (parse item)) pair))(cadr statement))) (parse (caddr statement))))
                  ((list? statement)
                   (cons 'list-exp (map (lambda (item) (parse item)) statement)))
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
                        (is_in_list (list '+ '- '* '/ '// '%) op)
                         )       
  )
                          
                          
                          

(provide (all-defined-out))