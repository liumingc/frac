(import (nanopass))

(define-language L1
  (terminals
    (symbol (x))
    (const (d))
    (prim (pr)))
  (Expr (e body)
    x
    d
    pr

    (or e* ...)
    (and e* ...)
    (not e0)

    (if e0 e1 e2)
    (if e0 e1)
    (e0 e* ...)
    (quote e0)
    (lambda (x* ...) body* ... body)
    (let ([x* e*] ...)
      body* ... body)
    (letrec ([x* e*] ...)
      body* ... body)
    (begin e* ... e)
    (set! x e)

    ))

(define const?
  (lambda (x)
    (or (number? x)
        (string? x)
        (boolean? x)
        (char? x)
        (null? x)
        )))

(define prim?
  (lambda (x)
    (memq x '(+ - * / % = < >
              cons car cdr null?))))

(define-parser parse-L1 L1)

(define gensym
  (let ([n 0])
    (case-lambda
      [()
        (gensym 'tmp)]
      [(x)
        (let ([x (if (symbol? x) (symbol->string x) x)])
          (let ([on n])
            (set! n (+ n 1))
            (string->symbol
              (string-append x (number->string on)))))])))

(define-pass rename : L1 (ir) -> L1 ()
  (definitions
    (define env '())
    (define extend
      (lambda (x e r)
        (cons (cons x e) r)))
    (define extend*
      (lambda (x* e* r)
        (if (null? x*)
            r
            (cons (cons (car x*) (car e*))
              (extend* (cdr x*) (cdr e*) r)))))
    )
  (Expr : Expr (ir r) -> Expr ()
    [(lambda (,x* ...) ,body* ... ,body)
      (let ([nx* (map (lambda (x) (gensym x)) x*)])
        (let* ([r (extend* x* nx* r)]
               [body* (map (lambda (x) (Expr x r)) body*)])
          `(lambda (,nx* ...)
             ,body* ...
             ,(Expr body r))))]
    [(let ([,x* ,[e* r -> e*]] ...) ,body* ... ,body)
      (let ([nx* (map (lambda (x) (gensym x)) x*)])
        (let* ([r (extend* x* nx* r)]
               [body* (map (lambda (x) (Expr x r)) body*)])
          `(let ([,nx* ,e*] ...)
             ,body* ...
             ,(Expr body r))))]
    [(letrec ([,x* ,[e* r -> e*]] ...) ,body* ... ,body)
      (let ([nx* (map (lambda (x) (gensym x)) x*)])
        (let ([r (extend* x* nx* r)])
          `(letrec ([,nx* ,e*] ...)
             ,(map (lambda (x)
                     (Expr x r)) body*)
             ,(Expr body r))))]
    [,x
     (let ([ans (assq x r)])
       (if ans
           (cdr ans)
           x))]
    )
  (Expr ir '()))

     

(define-language L2
  (extends L1)
  (Expr (e body)
    (- (if e0 e1)
       )))

(define-pass remove-one-armed-if : L1 (ir) -> L2 ()
  (Proc : Expr (ir) -> Expr ()
    [(if ,e0 ,e1)
     `(if ,e0 ,e1 #f)])
  (Proc ir))

(define-language L3
  (extends L2)
  (Expr (e body)
    (- (lambda (x* ...) body* ... body)
       (let ([x* e*] ...) body* ... body)
       (letrec ([x* e*] ...) body* ... body))
    (+ (lambda (x* ...) body)
       (let ([x* e*] ...) body)
       (letrec ([x* e*] ...) body))))

(define-pass make-explicit-begin : L2 (ir) -> L3 ()
  (Proc : Expr (ir) -> Expr ()
    [(lambda (,x* ...) ,[body*] ... ,[body])
     (if (null? body*)
         `(lambda (,x* ...) ,body)
         `(lambda (,x* ...)
            (begin ,body* ... ,body)))]
    [(let ([,x* ,[e*]] ...) ,[body*] ... ,[body])
      `(let ([,x* ,e*] ...)
         ,(if (null? body*)
              `,body
              `(begin ,body* ... ,body)))]
    [(letrec ([,x* ,[e*]] ...) ,[body*] ... ,[body])
      `(letrec ([,x* ,e*] ...)
         ,(if (null? body*)
              `,body
              `(begin ,body* ... ,body)))]
    ))

(define-language L4
  (extends L3)
  (Expr (e body)
    (- (or e* ...)
       (and e* ...)
       (not e0))))

(define-pass reduce-logic : L3 (ir) -> L4 ()
  (Expr : Expr (ir) -> Expr ()
    [(or ,[e*] ...)
     (if (null? e*)
         `#f
         (let f ([e* e*])
           (if (null? e*)
               '#f
               (let ([t (gensym)])
                 `(let ([,t ,(car e*)])
                    (if ,t ,t ,(f (cdr e*))))))))]
    [(and ,[e*] ...)
     (if (null? e*)
         #t
         (let f ([e0 (car e*)] [e* (cdr e*)])
           (if (null? e*)
               e0
               `(if ,e0
                    ,(f (car e*) (cdr e*))
                    #f))))]
    [(not ,e0)
     `(if ,e0 #f #t)]
    ))



(define convert 
  (lambda (sexp)
    (let ([x (unparse-L4
               (reduce-logic
               (make-explicit-begin
                 (remove-one-armed-if
                   (rename (parse-L1 sexp))))))])
      (pretty-print x)
      (newline)
      (newline)
      x)))

(define run
  (lambda (x)
    (let ([x (convert x)])
      (eval x))))

;;; do some test
(let ()
  (run '(if 3 4))
  (run '(let ([x 4])
          (+ x 4)
          (- y 6)))
  (run '(let ([x (lambda (x) (+ x 1))])
          (x 7)))

  (run '(or 3 4 5))
  (run '(or))
  (run '(or (or 3 4) (+ (or 5) (or 6 7))))
  )

#!eof

(unparse-L2 (remove-one-armed-if (parse-L1 '(if 3 4))))
