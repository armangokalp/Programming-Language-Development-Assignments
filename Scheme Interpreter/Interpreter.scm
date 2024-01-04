(define s7-interpret
  (lambda (exp env)
    (cond
      ((number? exp) exp)
      ((symbol? exp) (get-value exp env))
      ((list? exp)
       (case (car exp)
         ((define) (handle-define exp env))
         ((if) (handle-if exp env))
         ((let) (handle-let exp env))
         ((lambda) (handle-lambda exp env))
         (else (handle-application exp env))))
      (else (begin (display "") "ERROR")))))

(define initialize-global-env
  (lambda ()
    (list (cons '+ +)
          (cons '- -)
          (cons '* *)
          (cons '/ /))))

(define built-in-procedure?
  (lambda (proc)
    (or (eq? proc +)
        (eq? proc -)
        (eq? proc *)
        (eq? proc /))))


(define get-value
  (lambda (var env)
    (if (null? env)
        (display "")
        (if (eq? var (caar env))
            (cdar env)
            (get-value var (cdr env))))))

(define extend-env
  (lambda (vars vals old-env)
    (if (null? vars)
        old-env
        (extend-env (cdr vars) 
                    (cdr vals) 
                    (cons (cons (car vars) (car vals)) old-env)))))

(define handle-define
  (lambda (exp env)
    (if (or (not (pair? exp))
            (not (= (length exp) 3))
            (not (symbol? (cadr exp))))
        (begin (display "") "ERROR")
        (let ((var (cadr exp))
              (value (s7-interpret (caddr exp) env)))
          (extend-env (list var) (list value) env)
          var))))



(define handle-if
  (lambda (exp env)
    (if (or (not (pair? exp))
            (< (length exp) 4)
            (> (length exp) 4))
        (begin (display "") "ERROR")
        (let ((test-expr (s7-interpret (cadr exp) env)))
          (if (not (zero? test-expr))
              (s7-interpret (caddr exp) env)
              (s7-interpret (cadddr exp) env))))))


(define handle-let
  (lambda (exp env)
    (if (or (not (pair? exp))
            (< (length exp) 3)
            (not (list? (cadr exp))))
        (begin (display "") "ERROR")
        (let ((bindings (cadr exp))
              (body (caddr exp)))
          (if (or (any invalid-binding? bindings)
                  (not (unique-vars? bindings)))
              (begin (display "") "ERROR")
              (let loop ((binds bindings) (new-env env))
                (if (null? binds)
                    (s7-interpret body new-env)
                    (let ((bind (car binds)))
                      (let ((var (car bind))
                            (val (s7-interpret (cadr bind) env)))
                        (loop (cdr binds) (extend-env (list var) (list val) new-env)))))))))))

(define unique-vars?
  (lambda (bindings)
    (let loop ((vars '()) (binds bindings))
      (if (null? binds)
          #t
          (let ((var (caar binds)))
            (if (member var vars)
                #f
                (loop (cons var vars) (cdr binds))))))))

(define any
  (lambda (predicate lst)
    (cond ((null? lst) #f)
          ((predicate (car lst)) #t)
          (else (any predicate (cdr lst))))))


(define invalid-binding?
  (lambda (bind)
    (or (not (pair? bind))
        (not (= (length bind) 2))
        (not (symbol? (car bind))))))



(define handle-lambda
  (lambda (exp env)
    (if (or (not (pair? exp))
            (< (length exp) 3))
        (begin (display "") "ERROR")
        (let ((params (cadr exp))
              (body (caddr exp)))
          (if (not (list? params))
              (begin (display "") "ERROR")
              (lambda (args)
                (let ((extended-env (extend-env params args env)))
                  (s7-interpret body extended-env))))))))



(define handle-application
  (lambda (exp env)
    (let ((proc (s7-interpret (car exp) env))
          (operands (cdr exp)))
      (if (procedure? proc)
          (let ((evaluated-operands (map (lambda (arg) (s7-interpret arg env)) operands)))
            (if (built-in-procedure? proc)
                (apply proc evaluated-operands)
                (proc evaluated-operands)))
          (begin (display "") "ERROR")))))





(define define-expression?
  (lambda (exp)
    (and (list? exp)
         (eq? (car exp) 'define)
         (= (length exp) 3))))

(define get-define-var
  (lambda (exp)
    (if (define-expression? exp)
        (cadr exp)
        (begin (display "") "ERROR"))))

(define get-define-val
  (lambda (exp)
    (if (define-expression? exp)
        (caddr exp)
        (begin (display "") "ERROR"))))

(define cs305
  (lambda ()
    (let repl ((env (initialize-global-env)))
      (display "cs305> ")
      (let ((input (read)))
        (unless (eof-object? input)
          (let* ((result (if (define-expression? input)
                             (let ((var (get-define-var input))
                                   (val (s7-interpret (get-define-val input) env)))
                               (set! env (extend-env (list var) (list val) env))
                               var)
                             (s7-interpret input env))))
            (display "cs305: ")
            (cond ((eq? result 'error-define-syntax) (display "ERROR"))
                  ((number? result) (display result))
                  ((procedure? result) (display "[PROCEDURE]"))
                  ((symbol? result) (display result))
                  ((null? result) (display ""))
                  (else (display "ERROR")))
            (newline)
            (repl env)))))))

