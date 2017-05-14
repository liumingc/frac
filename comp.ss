(import (nanopass))

(define-language L1
  (terminals
    (symbol (x))
    (const (c))
    (prim (pr)))
  (Expr (e body)
    x
    c
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

(define prim-tbl
  '((+ . 2)
    (- . 2)
    (* . 2)
    (/ . 2)
    (% . 2)
    (= . 2)
    (< . 2)
    (> . 2)
    (cons . 2)
    (car . 1)
    (cdr . 1)
    (null? . 1)))

(define prim?
  (lambda (x)
    (assq x prim-tbl)))
    ;(memq x '(+ - * / % = < >
    ;          cons car cdr null?))))

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


;;; prim to primcall
(define-language L5
  (extends L4)
  (Expr (e body)
    (- pr)
    (+ (primcall pr e* ...))))

;;; introduce primcall
(define-pass inverse-eta : L4 (ir) -> L5 ()
  (Proc : Expr (ir) -> Expr ()
    [(,pr ,[e*] ...)
     `(primcall ,pr ,e* ...)]
    [,pr
     (let ([as (assq pr prim-tbl)])
       (let f ([i (cdr as)] [x* '()])
         (if (= i 0)
             `(lambda (,x* ...) (primcall ,pr ,x* ...))
             (f (- i 1) (cons (gensym) x*)))))]))

;;;
(define-language L6
  (extends L5)
  (Expr (e body)
    (- c
       (quote e0))
    (+ (quote d)))
  (Datum (d)
    (+ c e)))

#;(define datum?
  (lambda (d)
    (or (const? d)
        (and (pair? d)
             (and (datum? (car d))
                  (datum? (cdr d)))))))

(echo-define-pass quote-const : L5 (ir) -> L6 ()
  (Proc : Expr (ir) -> Expr ()
    [,c `(quote ,c)]))

;;; remove complex quote
;;; '(3 4) => (cons '3 '4)
;;; and <code> (eq? '(3 4) '(3 4)) => #f </code>
(define-language L7
  (extends L6)
  (Expr (e body)
    (- (quote d))
    (+ (quote c)))
  (Datum (d)
    (- e c)))

(define-pass remove-complex-quote : L6 (ir) -> L7 ()
  (Expr : Expr (ir) -> Expr ()
    [(quote ,d)
     (nanopass-case (L6 Datum) d
       [,c `(quote ,c)]
       [(,e0 ,e* ...)
        (let f ([e0 (Expr e0)]
                [e* (map Expr e*)])
          (if (null? e*)
              `(cons ,e0 '())
              `(cons ,e0 ,(f (car e*) (cdr e*)))))])]))

(define convert 
  (lambda (sexp)
    (let ([passes (list
                    parse-L1
                    rename
                    remove-one-armed-if
                    make-explicit-begin
                    reduce-logic
                    inverse-eta
                    quote-const
                    remove-complex-quote
                    )])
      (let f ([passes passes] [ir sexp])
        (if (null? passes)
            (unparse-L7 ir)
            (f (cdr passes) ((car passes) ir)))))))

(define run
  (lambda (x)
    (let ([x (convert x)])
      ;(eval x)
      x
)))

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
