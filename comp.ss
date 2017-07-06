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
    (<= . 2)
    (> . 2)
    (>= . 2)
    (cons . 2)
    (car . 1)
    (cdr . 1)
    (set-car! . 2)
    (set-cdr! . 2)
    (null? . 1)
    (pair? . 1)
    (void . 0)

    ;;; closure prim
    (make-clo . 1)
    (clo-code-set! . 2)
    (clo-data-set! . 3)
    (clo-code . 1)
    (clo-data . 2)
    ))

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
    [(let ([,x* ,[e*]] ...) (assigned (,a* ...) ,[body]))
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

(define-language L10
  (extends L9)
  (Expr (e body)
    (- le)))

(define-pass remove-anonymous-lambda : L9 (ir) -> L10 ()
  (Proc : Expr (e) -> Expr ()
    [(lambda (,x* ...) ,[abody])
     (let ([f (gensym 'anony)]) 
       `(letrec ([,f (lambda (,x* ...) ,abody)])
          ,f))]))

(define-language L11
  (extends L10)
  (terminals
    (- (symbol (x a)))
    (+ (symbol (x))))
  (Expr (e body)
    (- (let ([x* e*] ...) abody)
       (set! x e))
    (+ (let ([x* e*] ...) body)))
  (Abody (abody)
    (- (assigned (a* ...) body)))
  (LambdaExpr (le)
    (- (lambda (x* ...) abody))
    (+ (lambda (x* ...) body))))

(define-pass convert-assignments : L10 (ir) -> L11 ()
  (definitions
    (define build-let
      (lambda (x* e* body)
        (with-output-language (L11 Expr)
          (if (null? x*)
              body
              `(let ([,x* ,e*] ...) ,body)))))
    (define lookup
      (lambda (x r)
        (let ([ans (assq x r)])
          (if ans (cdr ans) x))))
    )
  (Proc : Expr (e r) -> Expr ()
    [(let ([,x* ,[e*]] ...) (assigned (,a* ...) ,body))
     (let ([t* (map (lambda (a) (gensym)) a*)])
       (let ([r (append (map cons a* t*) r)])
         (build-let (map (lambda (x) (lookup x r)) x*) e*
           (build-let a* (map (lambda (t) `(primcall cons ,t '#f)) t*)
             (Proc body r)))))]
    [(set! ,x ,[e r -> e])
     `(primcall set-car! ,x ,e)]
    [,x (guard (assq x r)) `(primcall car ,x)])
  (LambdaExpr : LambdaExpr (le r) -> LambdaExpr ()
    [(lambda (,x* ...) (assigned (,a* ...) ,body))
     (let ([t* (map (lambda (a) (gensym)) a*)])
       (let ([r (append (map cons a* t*) r)])
         `(lambda (,(map (lambda (x) (lookup x r)) x*) ...)
            ,(build-let a* (map (lambda (t) `(primcall cons ,t '#f)) t*)
               (Proc body r)))))])

  (Proc ir '()))

(define-language L12
  (extends L11)
  (terminals
    (- (symbol (x)))
    (+ (symbol (x f))))
  (LambdaExpr (le)
    (- (lambda (x* ...) body))
    (+ (lambda (x* ...) fbody)))
  (FreeBody (fbody)
    (+ (free (f* ...) body))))

(define-pass uncover-free : L11 (ir) -> L12 ()
  (Expr : Expr (e) -> Expr (free*)
    [(let ([,x* ,[e* f**]] ...) ,[body f*])
     (values
       `(let ([,x* ,e*] ...) ,body)
       (set:diff (apply set:union (cons f* f**)) x*))]
    [(letrec ([,x* ,[le* f**]] ...) ,[body f*])
     (values
       `(letrec ([,x* ,le*] ...) ,body)
       (set:diff (apply set:union (cons f* f**)) x*))]
    [(primcall ,pr ,[e* f**] ...)
     (values `(primcall ,pr ,e* ...) (apply set:union f**))]
    [(quote ,c) (values `(quote ,c) '())]
    [(if ,[e0 f0*] ,[e1 f1*] ,[e2 f2*]) (values `(if ,e0 ,e1 ,e2) (set:union f0* f1* f2*))]
    [(,[e0 f0*] ,[e* f**] ...)
     (values `(,e0 ,e* ...) (apply set:union f0* f**))]
    [(begin ,[e* f**] ... ,[e f*])
     (values `(begin ,e* ... ,e) (apply set:union f* f**))]
    [,x (values x (list x))]
    )
  (LambdaExpr : LambdaExpr (le) -> LambdaExpr (free*)
    [(lambda (,x* ...) ,[body f*])
     (let ([f* (set:diff f* x*)])
       (values `(lambda (,x* ...) (free (,f* ...) ,body)) f*))])
  (let-values ([(ir free*) (Expr ir)])
    ir))

(define-language L13
  (extends L12)
  (terminals
    (- (symbol (x f)))
    (+ (symbol (x f l))))
  (Expr (e body)
    (- (letrec ([x* le*] ...) body))
    (+ (closures ([x* l* f** ...] ...) lbody)
       (label l)))
  (LabelsBody (lbody)
    (+ (labels ([l* le*] ...) body))))

(define-pass convert-closures : L12 (ir) -> L13 ()
  (Expr : Expr (e) -> Expr ()
    [(letrec ([,x* ,[le*]] ...) ,[body])
     (let ([l* (map (lambda (x)
                      (let ([l (string-append "l:" (symbol->string x))])
                        (gensym l))) x*)]
           [cp* (map (lambda (x)
                       (gensym 'cp)) x*)])
       (let ([clo* (map (lambda (le cp)
                         (nanopass-case (L13 LambdaExpr) le
                           [(lambda (,x* ...) (free (,f* ...) ,body))
                            (cons (with-output-language (L13 LambdaExpr)
                                      `(lambda (,cp ,x* ...)
                                         (free (,f* ...) ,body)))
                              f*)]))
                     le* cp*)])
         (let ([f** (map cdr clo*)]
               [le* (map car clo*)])
           `(closures ([,x* ,l* ,f** ...] ...)
              (labels ([,l* ,le*] ...) ,body)))))]
    [(,x ,[e*] ...)
     `(,x ,x ,e* ...)]
    [(,[e] ,[e*] ...)
     (let ([t (gensym)])
       `(let ([,t ,e])
          (,t ,t ,e* ...)))]))

;;; TODO optimize-known-call

(define-language L14
  (extends L13)
  (Expr (e body)
    (- (closures ([x* l* f** ...] ...) lbody))
    (+ (labels ([l* le*] ...) body)))
  (LabelsBody (lbody)
    (- (labels ([l* le*] ...) body)))
  (LambdaExpr (le)
    (- (lambda (x* ...) fbody))
    (+ (lambda (x* ...) body)))
  (FreeBody (fbody)
    (- (free (f* ...) body)))
  )

(define-pass expose-clo-prims : L13 (ir) -> L14 ()
  (definitions
    (define build-clo-set
      (lambda (x* l* f** cp free*)
        (let ([o
        (with-output-language (L14 Expr)
          (fold-left
            (lambda (e* x l f*)
              (let lp ([f* f*] [e* e*] [i 0])
                (if (null? f*)
                    (cons `(primcall clo-code-set! ,x ,l) e*)
                    (lp (cdr f*)
                      (cons `(primcall clo-data-set! ,x ',i ,(handle-clo-ref (car f*) cp free*))
                        e*)
                      (+ i 1)))))
            '() x* l* f**))])
          ;(printf "~s~%" (map (lambda (x) (unparse-L14 x)) o))
          #;
          (for-each (lambda (x)
                      (printf "~s~%" (pretty-print (unparse-L14 x)))) o)
          o
          )))
    (define handle-clo-ref
      (lambda (f cp free*)
        (with-output-language (L14 Expr)
          (let lp ([free* free*] [i 0])
            (cond
              [(null? free*) f]
              [(eq? f (car free*)) `(primcall clo-data ,cp ',i)]
              [else (lp (cdr free*) (+ i 1))])))))
    )
  (Expr : Expr (e cp free*) -> Expr ()
    [(closures ([,x* ,l* ,f** ...] ...)
       (labels ([,l1* ,[le*]] ...) ,[body]))
      (let ([size* (map (lambda (f*) (length f*)) f**)])
        `(let ([,x* (primcall make-clo ',size*)] ...)
           ;,body
           (labels ([,l1* ,le*] ...)
             (begin
               ,(build-clo-set x* l* f** cp free*) ...
               ,body))))]
    [(,[e] ,[e*] ...) `((primcall clo-code ,e) ,e* ...)]
    [,x (handle-clo-ref x cp free*)])
  (LambdaExpr : LambdaExpr (le) -> LambdaExpr ()
    [(lambda (,x ,x* ...) (free (,f* ...) ,[body x f* -> body]))
     `(lambda (,x ,x* ...) ,body)]
    )
  (Expr ir #f '()))

(define-language L15
  (extends L14)
  (entry Program)
  (Program (p)
    (+ (labels ([l* le*] ...) l)))
  (Expr (e body)
    (- (labels ([l* le*] ...) body))))

(define-pass lift-lambdas : L14 (ir) -> L15 ()
  (definitions
    (define cl* '())
    (define cle* '()))
  (Expr : Expr (e) -> Expr ()
    [(labels ([,l* ,[le*]] ...) ,[body])
     (set! cl* (append l* cl*))
     (set! cle* (append le* cle*))
     body])
  (let ([ir (Expr ir)])
    (with-output-language (L15 Program)
      `(labels ([,cl* ,cle*] ... [l:main (lambda () ,ir)])
         l:main))))

(define-language L16
  (extends L15)
  (entry Program)
  (Expr (e body)
    (- (primcall pr e* ...)
       (e0 e* ...)
       x
       'c
       (label l))
    (+ (primcall pr se* ...) => (pr se* ...)
       (se0 se* ...)
       se))
  (SimpleExpr (se)
    (+ x
       'c
       (label l)))
  )

(define-pass remove-complex-opera* : L15 (ir) -> L16 ()
  (definitions
    (define simple1?
      (lambda (x)
        (nanopass-case (L16 Expr) x
          [,x #t]
          [',c #t]
          [(label ,l) #t]
          [else #f])))
    (define simple?
      (lambda (x)
        (let ([a (simple1? x)])
          ;(printf "~s simple? ~s~%" (unparse-L16 x) a)
          a)))
    (define convert-e*
      (lambda (e* f)
        (let lp ([e* e*] [x* '()] [xe* '()] [ans '()])
          (if (null? e*)
              (begin
                ;(printf "x*: ~s~% xe*:~s~% ans:~s~%~%" x* xe* (map unparse-L16 ans))
              (f x* xe* (reverse ans)))
              (let ([e (car e*)])
                (if (simple? e)
                    (lp (cdr e*) x* xe* (cons e ans))
                    (let ([x (gensym 's)])
                      (lp (cdr e*) (cons x x*) (cons e xe*) (cons x ans)))))))))
    )
  (Expr : Expr (e) -> Expr ()
    [(primcall ,pr ,[e*] ...)
     (convert-e* e*
       (lambda (x* xe* ans)
         (if (null? x*)
             `(primcall ,pr ,ans ...)
             `(let ([,x* ,xe*] ...)
                (primcall ,pr ,ans ...)))))]
    [(,[e0] ,[e*] ...)
     (convert-e* (cons e0 e*)
       (lambda (x* xe* ans)
         (if (null? x*)
             ans
             `(let ([,x* ,xe*] ...)
                (,(car ans) ,(cdr e*) ...)
                ))))]))

(define effect-prim-tbl
  '((set-car! . 2)
    (set-cdr! . 2)
    (clo-code-set! . 2)
    (clo-data-set! . 3)))

(define value-prim-tbl
  '((+ . 2)
    (- . 2)
    (* . 2)
    (/ . 2)
    (% . 2)
    (cons . 2)
    (car . 1)
    (cdr . 1)
    (make-clo . 1)
    (clo-code . 1)
    (clo-data . 2)
    (void . 0)
    ))

(define pred-prim-tbl
  '((null? . 1)
    (pair? . 1)
    (eq? . 2)
    (= . 2)
    (< . 2)
    (<= . 2)
    (> . 2)
    (>= . 2)
    ))

(define effect-prim?
  (lambda (x)
    (assq x effect-prim-tbl)))
(define value-prim?
  (lambda (x)
    (assq x value-prim-tbl)))
(define pred-prim?
  (lambda (x)
    (assq x pred-prim-tbl)))

(define-language L17
  (terminals
    (symbol (x l))
    (const (c))
    (effect-prim (epr))
    (value-prim (vpr))
    (pred-prim (ppr)))
  (entry Program)
  (Program (prog)
    (labels ([l* le*] ...) l))
  (LambdaExpr (le)
    (lambda (x* ...) body))
  (SimpleExpr (se)
    x
    (quote c)
    (label l))
  (Value (v body)
    se
    (se se* ...)
    (primcall vpr se* ...) => (vpr se* ...)
    (if p0 v1 v2)
    (begin e* ... v)
    (let ([x* v*] ...) v))
  (Effect (e)
    (nop)
    (primcall epr se* ...) => (epr se* ...)
    (if p0 e1 e2)
    (begin e* ... e)
    (let ([x* v*] ...) e)
    (se se* ...))
  (Pred (p)
    (true)
    (false)
    (primcall ppr se* ...) => (ppr se* ...)
    (if p0 p1 p2)
    (begin e* ... p)
    (let ([x* v*] ...) p)
    )
  )

(define-pass recognize-context : L16 (ir) -> L17 ()
  (Value : Expr (e) -> Value ()
    [(primcall ,pr ,[se*] ...)
     (guard (value-prim? pr))
     `(primcall ,pr ,se* ...)]
    [(primcall ,pr ,[se*] ...)
     (guard (pred-prim? pr))
     `(if (primcall ,pr ,se* ...) (true) (false))]
    [(primcall ,pr ,[se*] ...)
     (guard (effect-prim? pr))
     `(begin (primcall ,pr ,se* ...) (primcall void))]
    [(if ,[p0] ,[v1] ,[v2])
     (nanopass-case (L17 Pred) p0
       [(true) v1]
       [(false) v2]
       [else `(if ,p0 ,v1 ,v2)])]
    )
  (Effect : Expr (e) -> Effect ()
    [(primcall ,pr ,[se*] ...)
     (guard (or (value-prim? pr) (pred-prim? pr)))
     `(nop)]
    [(primcall ,pr ,[se*] ...)
     (guard (effect-prim? pr))
     `(primcall ,pr ,se* ...)]
    [,se `(nop)]
    )
  (Pred : Expr (e) -> Pred ()
    [(quote ,c) (if c `(true) `(false))]
    [,se
     `(if (primcall eq? ,se '#f)
          (false)
          (true))]
    [(if ,[p0] ,[p1] ,[p2])
     (nanopass-case (L17 Pred) p0
       [(true) p1]
       [(false) p2]
       [else `(if ,p0 ,p1 ,p2)])]
    [(,[se] ,[se*] ...)
     (let ([t (gensym)])
       `(let ([,t (,se ,se* ...)])
          (if ,t (true) (false))))]
    [(primcall ,pr ,[se*] ...)
     (guard (pred-prim? pr))
     `(primcall ,pr ,se* ...)]
    [(primcall ,pr ,[se*] ...)
     (guard (effect-prim? pr))
     `(begin (primcall ,pr ,se* ...) (true))]
    [(primcall ,pr ,[se*] ...)
     (guard (value-prim? pr))
     (let ([t (gensym)])
       `(let ([,t (primcall ,pr ,se* ...)])
          (if (primcall eq? ,t '#f) (false) (true))))]
  ))


(define i64?
  (lambda (x)
    (and (number? x)
         (<= (- (expt 2 63)) x)
         (< x (expt 2 63)))))

(define-language L18
  (extends L17)
  (terminals
    (+ (i64 (i))))
  (Value (v body)
    (+ (alloc i se))))

(define fixnum-tag #b000)
(define pair-tag #b001)
(define closure-tag   #b100)
(define boolean-tag   #b1101)
(define true-rep    #b111101)
(define false-rep   #b101101)
(define null-rep    #b100101)
(define void-rep    #b110101)
(define type-mask   #b111)
(define word-size   8)


(define-pass expose-alloc-primitives : L17 (ir) -> L18 ()
  (Value : Value (e) -> Value ()
    [(primcall ,vpr ,[se1] ,[se2])
     (guard (eq? vpr 'cons))
     (let ([t1 (gensym)] [t2 (gensym)] [t3 (gensym)])
       `(let ([,t1 (alloc ,pair-tag '2)]
              [,t2 ,se1]
              [,t3 ,se2])
          (begin
            (primcall set-car! ,t1 ,t2)
            (primcall set-cdr! ,t1 ,t3)
            ,t1)))]
    [(primcall ,vpr ,[se])
     (guard (eq? vpr 'make-clo))
     ;(printf "input:~s, c:~s~%" (pretty-print (unparse-L17 e)) (pretty-print (unparse-L17 se)))
     (let ([n (nanopass-case (L18 SimpleExpr) se
                [(quote ,c) c])])
       `(alloc ,closure-tag (quote ,(+ n 1))))]))

(define-language L19
  (extends L18)
  (LambdaExpr (le)
    (- (lambda (x* ...) body))
    (+ (lambda (x* ...) lbody)))
  (LocalsBody (lbody)
    (+ (local (x* ...) body)))
  (Value (v body)
    (- (let ([x* v*] ...) v)))
  (Effect (e)
    (- (let ([x* v*] ...) e))
    (+ (set! x v)))
  (Pred (p)
    (- (let ([x* v*] ...) p))))

(define-pass return-of-set! : L18 (ir) -> L19 ()
  #;
  (definitions
    (define build-set!
      (lambda (x* v* body build-begin)
        (with-output-language (L19 Effect)
          (build-begin
            (map (lambda (x v) `(set! ,x ,v)) x* v*)
            body)))))
  (LambdaExpr : LambdaExpr (le) -> LambdaExpr ()
    ; l for local
    [(lambda (,x* ...) ,[body l*])
      `(lambda (,x* ...)
         (local (,l* ...) ,body))])
  (Value : Value (e) -> Value ('())
    [(let ([,x* ,[v* l1**]] ...)
       ,(v l2*))
     (let ([out-l* (apply append x* l2* l1**)])
       (values
         `(begin (set! ,x* ,v*) ... ,v)
         out-l*))]
    [(,(se l*) ,(se* l**) ...)
     (values `(,se ,se* ...) (apply append l* l**))]
    [(primcall ,vpr ,(se* l**) ...)
     (values `(primcall ,vpr ,se* ...) (apply append l**))]
    [(if ,(p0 l0*) ,(v1 l1*) ,(v2 l2*))
     (values `(if ,p0 ,v1 ,v2) (append l0* l1* l2*))]
    [(begin ,(e* l**) ... ,(v l*))
     (values `(begin ,e* ... ,v) (apply append l* l**))]
    )
  (SimpleExpr : SimpleExpr (se) -> SimpleExpr ('()))
  (Effect : Effect (e)  -> Effect ('())
    [(let ([,x* ,[v* l1**]] ...)
       ,(e l*))
     (let ([out-l* (apply append x* l* l1**)])
       (values
         `(begin (set! ,x* ,v*) ... ,e)
         out-l*))]
    [(primcall ,epr ,(se* l**) ...)
     (values `(primcall ,epr ,se* ...) (apply append l**))]
    [(if ,(p0 l0*) ,(e1 l1*) ,(e2 l2*))
     (values `(if ,p0 ,e1 ,e2) (append l0* l1* l2*))]
    [(begin ,(e* l**) ... ,(e l*))
     (values `(begin ,e* ... ,e) (apply append l* l**))]
    )
  (Pred : Pred (e) -> Pred ('())
    [(let ([,x* ,[v* l1**]] ...)
       ,(p l1*))
     (let ([out-l* (apply append x* l1* l1**)])
       (values
         `(begin (set! ,x* ,v*) ... ,p)
         out-l*))]
    [(primcall ,ppr ,(se* l**) ...)
     (values `(primcall ,ppr ,se* ...) (apply append l**))]
    [(if ,(p0 l0*) ,(p1 l1*) ,(p2 l2*))
     (values `(if ,p0 ,p1 ,p2) (append l0* l1* l2*))]
    [(begin ,(e* l**) ... ,(p l*))
     (values `(begin ,e* ... ,p) (apply append l* l**))]
    )
  )

(define-language L20
  (extends L19)
  (Value (v body)
    (- (alloc i se)
       se
       (se se* ...)
       (primcall vpr se* ...))
    (+ rhs))
  (Rhs (rhs)
    (+ (alloc i se)
       se
       (se se* ...)
       (primcall vpr se* ...) => (vpr se* ...)
       ))
  (Effect (e)
    (- (set! x v))
    (+ (set! x rhs))))

;;;
(define-pass flatten-set! : L19 (ir) -> L20 ()
  (SimpleExpr : SimpleExpr (se) -> SimpleExpr ())

  (Effect : Effect (e) -> Effect ()
    [(set! ,x ,v)
     (flatten v x)])
  (flatten : Value (e x) -> Effect ()
    [(alloc ,i ,[se])
      `(set! ,x (alloc ,i ,se))]
    [,se `(set! ,x ,(SimpleExpr se))]
    [(,[se] ,[se*] ...) `(set! ,x (,se ,se* ...))]
    [(primcall ,vpr ,[se*] ...) `(set! ,x (primcall ,vpr ,se* ...))]
    ; the next line is not necessary, but to illustrated the auto generate pass
    ; code
    [(if ,[p0] ,[flatten : v1 x -> e1] ,[e2])
     `(if ,p0 ,e1 ,e2)]
    ))

(define-language L21
  (extends L20)
  (terminals
    (- (const (c))))
  (SimpleExpr (se)
    (- (quote c))
    (+ i)))

(define-pass convert-const-representation : L20 (ir) -> L21 ()
  (SimpleExpr : SimpleExpr (se) -> SimpleExpr ()
    [(quote ,c)
     (cond
       [(eq? c #f) false-rep]
       [(eq? c #t) true-rep]
       [(null? c) null-rep]
       [(i64? c) (bitwise-arithmetic-shift-left c 3)])]))

(define-language L22
  (extends L21)
  (Effect (e)
    (- (primcall epr se* ...))
    (+ (mset! se0 (maybe se1?) i se2)))
  (Pred (p)
    (- (primcall ppr se* ...))
    (+ (= se0 se1)
       (< se0 se1)
       (<= se0 se1)))
  (Rhs (rhs)
    (- (primcall vpr se* ...)))
  (SimpleExpr (se)
    (+ (mref se0 (maybe se1?) i)
       (add se0 se1)
       (sub se0 se1)
       (mul se0 se1)
       (div se0 se1)
       (sl se0 se1)
       (sr se0 se1)
       (logand se0 se1))))

(trace-define-pass expand-primitives : L21 (ir) -> L22 ()
  (Pred : Pred (p) -> Pred ()
    ;;; note: you can't write 
    ;;; (primcall null? se0)
    [(primcall ,ppr ,[se0])
     (case ppr
       [(null?) `(= ,se0 ,null-rep)]
       [(pair?) `(= (logand ,se0 ,type-mask) ,pair-tag)])]
    [(primcall ,ppr ,[se0] ,[se1])
     (case ppr
       [(eq?) `(= ,se0 ,se1)]
       [(=) `(= ,se0 ,se1)]
       [(<) `(< ,se0 ,se1)]
       [(<=) `(<= ,se0 ,se1)]
       [(>) `(<= ,se1 ,se0)]
       [(>=) `(< ,se1 ,se0)])]
    )

  (Effect : Effect (e) -> Effect ()
    [(primcall ,epr ,[se0] ,[se1])
     (case epr
       [(set-car!) `(mset! ,se0 #f ,(- pair-tag) ,se1)]
       [(set-cdr!) `(mset! ,se0 #f ,(- word-size pair-tag) ,se1)]
       [(clo-code-set!) `(mset! ,se0 #f ,(- closure-tag) ,se1)]
       )]
    [(primcall ,epr ,[se0] ,[se1] ,[se2])
     ;;; clo-data-set!
     `(mset! ,se0 ,se1 ,(- word-size closure-tag) se2)])

  (Rhs : Rhs (rhs) -> Rhs ()
    (definitions
      (define convert-op
        (let ([tbl '((+ . add) (- . sub)
                     (* . mul) (/ . div))])
          (lambda (x)
            (let ([t (assq x tbl)])
              (if t (cdr t) x))))))
    [(primcall ,vpr ,[se0] ,[se1])
     (case vpr
       [(+ - * / %)
        `(,(convert-op vpr) ,se0 ,se1)]
       [(clo-data)
        `(mref ,se0 ,se1 ,(- word-size closure-tag))]
       )]
    [(primcall ,vpr ,[se0])
     (case vpr
       [(car) `(mref ,se0 #f ,(- pair-tag))]
       [(cdr) `(mref ,se0 #f ,(- word-size pair-tag))]
       [clo-code `(mref ,se0 #f ,(- closure-tag))])]
    [(primcall ,vpr)
     ;(guard (eq? vpr 'void))
     void-rep]
    )
  
  )

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
              (,remove-anonymous-lambda . ,unparse-L10)
              (,convert-assignments . ,unparse-L11)
              (,uncover-free . unparse-L12)
              (,convert-closures . unparse-L13)
              (,expose-clo-prims . unparse-L14)
              (,lift-lambdas . unparse-L15)
              (,remove-complex-opera* . unparse-L16)
              (,recognize-context . unparse-L17)
              (,expose-alloc-primitives . unparse-L18)
              (,return-of-set! . unparse-L19)
              (,flatten-set! . unparse-L20)
              (,convert-const-representation . unparse-L21)
              (,expand-primitives . unparse-L22)
              )
            ])
      (let f ([passes passes] [ir sexp])
        (if (null? passes)
            (unparse-L22 ir)
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

  (run '(let ([foo (lambda (a b) (+ a b))]
              [x 3]
              [y 4])
          (letrec ([m 5] [n 6] [bar (lambda (x y) (- x y))])
            (+ (foo x y) (bar m n)))))
  (run '((lambda (x y) (+ x y)) 3 4))
  (run '(let ([foo (lambda (f a b) (f a b))])
          (foo + 3 4)))

  (run '(let ([x 3]
              [y 4])
          (set! x (+ x 5))
          (+ x y)))

  (run '(lambda (x y z)
          (set! x (+ x 3))
          (set! y (+ x z))
          (+ x y)))

  (run '(let ([n 0])
          (let ([f (lambda (x)
                     (set! n (+ n x))
                     n)])
            (f 5))))
  |#

  (run '(let ([n 0]
              [init 5])
          (let ([foo (lambda (x)
                       (set! n (+ n x))
                       n)]
                [bar (lambda (x)
                       (set! n (- n x))
                       n)]
                [reset (lambda ()
                         (set! n init))])
            (foo 3)
            (bar 1)
            (reset)
            n)))

  (run '(let ([x (let ([y 3]
                       [z 4])
                   (+ y z))]
              [m (let ([n 5])
                   (+ n 6))])
          (* x m)))


  )

#!eof

;(unparse-L3 (remove-one-armed-if (parse-L1 '(if 3 4))))
