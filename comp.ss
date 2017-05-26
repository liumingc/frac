(import (nanopass))

(define debug-pass #f)

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
    [(letrec ([,x* ,[e* r -> e*]] ...) ,[body* r -> body*] ... ,body)
      (let ([nx* (map (lambda (x) (gensym x)) x*)])
        (let ([r (extend* x* nx* r)])
          `(letrec ([,nx* ,e*] ...)
             ,body* ...
             ,(Expr body r))))]
    [(set! ,x ,[e])
     (let ([ans (assq x r)])
       (if ans
           `(set! ,(cdr ans) ,e)
           `(set! ,x ,e)))]
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

(define-pass quote-const : L5 (ir) -> L6 ()
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
              `(primcall cons ,e0 '())
              `(primcall cons ,e0 ,(f (car e*) (cdr e*)))))])]))

;;; find assigned var
(define-language L8
  (extends L7)
  (terminals
    (- (symbol (x)))
    (+ (symbol (x a))))
  (Expr (e body)
    (- (lambda (x* ...) body)
       (let ([x* e*] ...) body)
       (letrec ([x* e*] ...) body))
    (+ (lambda (x* ...) abody)
       (let ([x* e*] ...) abody)
       (letrec ([x* e*] ...) abody)))
  (Abody (abody)
    (+ (assigned (a* ...) body))))


(define set:cons
  (lambda (x s)
    (if (memq x s) s (cons x s))))

#;
(define set:union
  (lambda (s1 s2)
    (fold-right (lambda (x acc)
                  (set:cons x acc)) s2 s1)))

(define set:union
  (lambda set*
    (if (null? set*)
        '()
        (fold-right (lambda (seta acc)
                      (fold-right (lambda (x acc)
                                    (set:cons x acc))
                        acc seta))
          (car set*) (cdr set*)))))

(define set:diff
  (lambda (s1 s2)
    (let f ([s1 s1] [s '()])
      (if (null? s1)
          s
          (if (memq (car s1) s2)
              (f (cdr s1) s)
              (f (cdr s1) (cons (car s1) s)))))))


(define set:intersect
  (lambda (s1 s2)
    (fold-right (lambda (x acc)
                  (if (memq x s2)
                      (set:cons x acc)
                      acc)) '() s1)))
    

(define-pass identify-assigned : L7 (ir) -> L8 ()
  (Proc : Expr (ir) -> Expr ('())
    [(lambda (,x* ...) ,[body a*])
      (let ([this-a* (set:intersect a* x*)]
            [out-a* (set:diff a* x*)])
        (values `(lambda (,x* ...) (assigned (,this-a* ...) ,body)) out-a*))]
    [(set! ,x ,[e a*])
     (values `(set! ,x ,e) (set:cons x a*))]
    [(begin ,[e* a**] ... ,[e a*])
     (values `(begin ,e* ... ,e)
       (apply set:union a* a**))]
    [(let ([,x* ,[e* a**]] ...)
       ,[body a*])
     (values `(let ([,x* ,e*] ...)
                (assigned (,(set:intersect a* x*) ...) ,body))
       (apply set:union (set:diff a* x*) a**))]
    [(letrec ([,x* ,[e* a**]] ...)
       ,[body a*])
      (values `(letrec ([,x* ,e*] ...)
                 (assigned (,(set:intersect a* x*) ...) ,body))
        (apply set:union (set:diff a* x*) a**))]
    [(primcall ,pr ,[e* a**] ...)
     (values `(primcall ,pr ,e* ...) (apply set:union a**))]
    [(if ,[e0 a0] ,[e1 a1] ,[e2 a2])
     (values `(if ,e0 ,e1 ,e2) (set:union a0 a1 a2))]
    [(,[e0 a*] ,[e* a**] ...)
     (values `(,e0 ,e* ...) (apply set:union a* a**))]
    )
  (let-values ([(ir a*) (Proc ir)])
    #;
    (unless (null? a*)
      (error 'identify-assigned "found one or more unbound variables" a*))
    ir))


(define-language L9
  (extends L8)
  (Expr (e body)
    (- (letrec ([x* e*] ...)
         abody)
       (lambda (x* ...) abody))
    (+ (letrec ([x* le*] ...)
         body)
       le))
  (LambdaExpr (le)
    (+ (lambda (x* ...) abody))))


(define-pass purify-letrec : L8 (ir) ->  L9 ()
  (definitions
    (define build-let
      (lambda (x* e* a* body)
        (with-output-language (L9 Expr)
          (if (null? x*)
              body
              `(let ([,x* ,e*] ...) (assigned (,a* ...) ,body))))))
    (define build-letrec
      (lambda (x* e* body)
        (with-output-language (L9 Expr)
          (if (null? x*)
              body
              `(letrec ([,x* ,e*] ...) ,body)))))

    (define build-begin
      (lambda (e* e)
        (if (null? e*)
            e
            (with-output-language (L9 Expr)
              `(begin ,e* ... ,e)))))

    (define simple?
      ;;; (quote c) | (primcall ,pr, e* ...) | (begin ,e* ... e) |
      ;;; (if ,e0 ,e1 ,e2) | ,x
      (lambda (e)
        (nanopass-case (L9 Expr) e
          [(quote ,c) #t]
          [(primcall ,pr ,e* ...)
           (for-all simple? e*)]
          [(begin ,e* ... ,e)
           (and (for-all simple? e*) (simple e))]
          [(if ,e0 ,e1 ,e2) (and (simple? e0) (simple? e1) (simple? e2))]
          [,x #t]
          [else #f])))

    (define lambda?
      (lambda (e)
        (nanopass-case (L9 Expr) e
          [(lambda (,x* ...) ,abody) #t]
          [else #f])))
           
    )
  (Proc : Expr (ir) -> Expr ()
    [(letrec ([,x* ,[e*]] ...) (assigned (,a* ...) ,[body]))
     (let f ([xb* x*] [e* e*]
             [xs* '()] [es* '()] ; simple
             [xl* '()] [el* '()] ; letrec function
             [xc* '()] [ec* '()] ; complex
             )
       (if (null? xb*)
           (build-let xc* (make-list (length xc*) `(quote #f)) xc*
             (build-letrec xl* el*
               (build-let xs* es* '()
                 (build-begin
                   (map (lambda (xc xe)
                          `(set! ,xc ,xe)) xc* ec*)
                   body))))
           (let ([x (car xb*)] [e (car e*)])
             (cond
               [(lambda? e) (f (cdr xb*) (cdr e*) xs* es* (cons x xl*) (cons e el*) xc* ec*)]
               [(simple? e) (f (cdr xb*) (cdr e*) (cons x xs*) (cons e es*) xl* el* xc* ec*)]
               [else (f (cdr xb*) (cdr e*) xs* es* xl* el* (cons x xc*) (cons e ec*))]))))]))

;;; ((lambda (x* ...) abody) e* ...) -> (let ([x* e*] ...) abody)
(define-pass optimize-direct-call : L9 (ir) -> L9 ()
  (Proc : Expr (e) -> Expr ()
    [((lambda (,x* ...) ,[abody]) ,[e*] ...)
     `(let ([,x* ,e*] ...)
        ,abody)]))


(define-pass find-let-bound-lambdas : L9 (ir) -> L9 ()
  (Proc : Expr (e) -> Expr ()
    (definitions
      (define lambda?
        (lambda (e)
          (nanopass-case (L9 Expr) e
            [(lambda (,x* ...) ,abody) #t]
            [else #f])))
      (define build-letrec
        (lambda (x* e* body)
          (with-output-language (L9 Expr)
            (if (null? x*)
                body
                `(letrec ([,x* ,e*] ...) ,body)))))
      (define build-let
        (lambda (x* e* a* body)
          (with-output-language (L9 Expr)
            (if (null? x*)
                body
                `(let ([,x* ,e*] ...) (assigned (,a* ...) ,body))))))
      )
    [(let ([,x* ,e*] ...) (assigned (,a* ...) ,body))
     (let f ([x* x*] [e* e*]
             [xv* '()] [ev* '()]
             [xl* '()] [el* '()])
       (if (null? x*)
           (build-let xv* ev* a*
             (build-letrec xl* el* body))
           (cond
             [(lambda? (car e*))
              (f (cdr x*) (cdr e*) xv* ev* (cons (car x*) xl*) (cons (car e*) el*))]
             [else
              (f (cdr x*) (cdr e*) (cons (car x*) xv*) (cons (car e*) ev*) xl* el*)])))]))

(define convert 
  (lambda (sexp)
    (let ([passes 
            `((,parse-L1 . ,unparse-L1)
              (,rename . ,unparse-L1)
              (,remove-one-armed-if . ,unparse-L2)
              (,make-explicit-begin . ,unparse-L3)
              (,reduce-logic . ,unparse-L4)
              (,inverse-eta . ,unparse-L5)
              (,quote-const . ,unparse-L6)
              (,remove-complex-quote . ,unparse-L7)
              (,identify-assigned . ,unparse-L8)
              (,purify-letrec . ,unparse-L9)
              (,optimize-direct-call . ,unparse-L9)
              (,find-let-bound-lambdas . ,unparse-L9)
              )
            ])
      (let f ([passes passes] [ir sexp])
        (if (null? passes)
            (unparse-L9 ir)
            (let ([pass (car passes)])
              (let ([ir ((car pass) ir)])
                (if debug-pass
                    (begin
                      (pretty-print ((cdr pass) ir))
                      (newline)
                      (newline)))
                (f (cdr passes) ir))))))))



(define run
  (lambda (x)
    (let ([x (convert x)])
      ;(eval x)
      x
      (pretty-print x)
)))

;;; do some test
(let ()
  #|
  (run '(if 3 4))
  (run '(let ([x 4])
          (+ x 4)
          (- y 6)))
  (run '(let ([x (lambda (x) (+ x 1))])
          (x 7)))

  (run '(or 3 4 5))
  (run '(or))
  (run '(or (or 3 4) (+ (or 5) (or 6 7))))

  (run ''(3 4))

  (run '(letrec ([x 3]
                 [y 4]
                 [f (lambda (x y) (+ x y))]
                 [multiply (lambda (x y) (* x y))])
          (f (multiply x 7) (f y 8))))
  (run '((lambda (a b) (+ a b)) 3 4))
  |#

  (run '(let ([foo (lambda (a b) (+ a b))]
              [x 3]
              [y 4])
          (letrec ([m 5] [n 6] [bar (lambda (x y) (- x y))])
            (+ (foo x y) (bar m n)))))

  )

#!eof

;(unparse-L2 (remove-one-armed-if (parse-L1 '(if 3 4))))
