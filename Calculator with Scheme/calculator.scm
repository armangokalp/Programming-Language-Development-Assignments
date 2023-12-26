(define (twoOperatorCalculator expr)
  (define (eval-expr lst)
    (if (null? lst)
        0
        (let loop ((rest (cdr lst)) (acc (car lst)))
          (if (null? rest)
              acc
              (let ((op (car rest)) (next (cadr rest)))
                (loop (cddr rest)
                      (cond ((eq? op '+) (+ acc next))
                            ((eq? op '-) (- acc next))
                            (else acc))))))))
  (eval-expr expr))


(define (fourOperatorCalculator expr)
  (define (eval-op op a b)
    (cond ((eq? op '*) (* a b))
          ((eq? op '/) (/ a b))
          (else b)))

  (define (process-expr lst acc)
    (if (null? lst)
        acc
        (let ((item (car lst)))
          (cond ((number? item)
                 (if (and (pair? acc) (member (cadr acc) '(+ -)))
                     (process-expr (cdr lst) (cons (eval-op (cadr acc) (car acc) item) (cddr acc)))
                     (process-expr (cdr lst) (cons item acc))))
                ((member item '(+ -)) (process-expr (cdr lst) (cons item acc)))
                ((member item '(* /))
                 (let ((next (cadr lst)))
                   (process-expr (cddr lst) (cons (eval-op item (car acc) next) (cdr acc)))))
                (else acc)))))

  (reverse (process-expr (cdr expr) (list (car expr)))))



(define (calculatorNested expr)
  (define (eval-nested lst)
    (cond ((null? lst) '())
          ((list? (car lst)) 
           (cons (twoOperatorCalculator (fourOperatorCalculator (eval-nested (car lst)))) 
                 (eval-nested (cdr lst))))
          (else (cons (car lst) (eval-nested (cdr lst))))))

  (define (flatten lst)
    (cond ((null? lst) '())
          ((list? (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
          (else (cons (car lst) (flatten (cdr lst))))))

  (flatten (eval-nested expr)))



(define (checkOperators expr)
  (define (error-handler ex)
    #f)

  (define (valid-operators? lst last-item-was-number)
    (cond ((null? lst) last-item-was-number)
          ((list? (car lst)) 
           (and (valid-operators? (car lst) #f)
                (if (null? (cdr lst)) 
                    #t 
                    (valid-operators? (cdr lst) #t))))
          ((number? (car lst)) 
           (valid-operators? (cdr lst) #t))
          ((and (member (car lst) '(+ - * /)) last-item-was-number) 
           (if (and (pair? (cdr lst)) 
                    (or (number? (cadr lst)) (list? (cadr lst))))
               (valid-operators? (cdr lst) (number? (cadr lst)))
               #f))
         (else #f)))

  (with-exception-handler error-handler
    (lambda ()
      (if (list? expr)
          (valid-operators? expr #f)
          #f))))



(define (calculator expr)
  (define (error-handler ex)
    #f)

  (define (is-valid-expression? expr)
    (and (list? expr) (checkOperators expr)))

  (define (evaluate expr)
    (cond ((not (list? expr)) expr)
          ((is-single-element-list expr) (car expr))
          (else (twoOperatorCalculator (fourOperatorCalculator expr)))))

  (define (is-single-element-list lst)
      (and (list? lst) (= (length lst) 1)))

    (with-exception-handler error-handler
      (lambda ()
        (if (is-valid-expression? expr)
            (let ((result (calculatorNested expr)))
              (evaluate result))
            #f))))