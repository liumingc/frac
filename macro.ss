;; expander
(define true #t)
(define false #f)

(define exp
  (lambda (ir)
    (lambda (syntax-table)
      (let ([x1 ((timestamp ir) S0)])
        (printf "after timestamp=>~s\n" x1)
        (let ([x2 (((ev x1) syntax-table) 1)])
          (printf "after ev=>~s\n" x2)
          (let ([x3 (alpha-convert x2)])
            (printf "after alpha-convert=>~s\n" x3)
            (let ([x4 (strip x3)])
              (printf "after strip=>~s\n" x4)
              x4)))))))

(define timestamp
  (lambda (ir)
    (lambda (ts)
      (cond
        [(atomic-non-var? ir) ir]
        [(var? ir) (ts ir)]
        [else (map (lambda (x) ((timestamp x) ts)) ir)]))))

(define ev
  (lambda (ir)
    (lambda (syntax-table)
      (lambda (j)
        (cond
          [(const? ir) ir]
          [(stamped? ir) ir]
          [(quote? ir) ir]
          [(macro? ir)
           (let ([x1 (syntax-table ir)])
             (printf "  from ")
             (pretty-print ir)
             (display #\backspace)
             (printf " tao=>")
             (pretty-print x1)
             (newline)
             (let ([x2 ((timestamp x1) (S j))])
               (printf "  -add ts=>~s\n" x2)
               (((ev x2) syntax-table) (begin (set! j (+ j 1)) j))))]
          [(lambda? ir)
           `(lambda ,(var ir)
              ,(((ev (body ir)) syntax-table) j))]
          [(app? ir)
           `(,(((ev (fun ir)) syntax-table) j)
              ,(((ev (arg ir)) syntax-table) j))])))))

;; replace w with v
(define subst
  (lambda (v w)
    (lambda (ir)
      (cond
        [(stamped? ir)
         (if (eq? ir w) v ir)]
        [(atomic-not-stamped? ir) ir]
        [(quote? ir) ir]
        [(lambda? ir)
         (if (eq? w (var ir))
             `(lambda ,w ,(body ir))
             `(lambda ,(var ir)
                ,((subst v w) (body ir))))]
        [(app? ir)
         `(,((subst v w) (fun ir))
            ,((subst v w) (arg ir)))]))))

(define genvar
  (lambda (s n)
    (string->symbol
      (if (number? n)
          (string-append (symbol->string s) ":" (number->string n))
          (string-append (symbol->string s) ":" n)))))


(define alpha-convert
  (lambda (ir)
    (cond
      [(var? ir) ir]
      [(atomic-non-var? ir) ir]
      [(quote? ir) ir]
      [(lambda? ir)
        (let ([v (genvar (strip (var ir)) "new")])
          `(lambda ,v
             ,(alpha-convert ((subst v (var ir))
                               (body ir)))))]
      [(app? ir)
        `(,(alpha-convert (fun ir))
           ,(alpha-convert (arg ir)))])))


(define plist '())
(define put
  (lambda (var p v)
    (let ([r1 (assq var plist)])
      (if r1
          (let ([r2 (assq p (cdr r1))])
            (if r2
                (set-cdr! r2 v)
                (set-cdr! r1 (list (cons p v)))))
          (set! plist (cons (cons var (list (cons p v))) plist))))))

(define get
  (lambda (var p)
    (let ([r1 (assq var plist)])
      (if r1
          (let ([r2 (assq p (cdr r1))])
            (if r2
                (cdr r2)
                #f))
          #f))))

(define strip
  (lambda (ir)
    (cond
      [(atomic-not-stamped? ir) ir]
      [(stamped? ir) (get ir 'original-name)]
      [else (map strip ir)])))

(define S
  (lambda (n)
    (let ([seen '()])
      (lambda (v)
        (let ([info (assq v seen)])
          (if info
              (cdr info)
              (let ([new (genvar v n)])
                (put new 'original-name v)
                (set! seen
                  (cons (cons v new) seen))
                new)))))))

(define S0 (S 0))


(define stamped?
  (lambda (w)
    (and (symbol? w)
         (get w 'original-name))))

(define mactok?
  (lambda (m)
    (and (symbol? m)
         (get m 'mactok))))

(define coretok?
  (lambda (c)
    (and (symbol? c)
         (get c 'coretok))))

(define quote?
  (lambda (ir)
    (and (pair? ir)
         (eq? (car ir) 'quote)
         (pair? (cdr ir))
         (null? (cddr ir)))))

;; (lambda var body)
(define lambda?
  (lambda (ir)
    (and (pair? ir)
         (eq? (car ir) 'lambda)
         (pair? (cdr ir))
         (var? (cadr ir))
         (pair? (cddr ir))
         (null? (cdddr ir)))))

;; (fun arg)
(define app?
  (lambda (ir)
    (and (pair? ir)
         (pair? (cdr ir))
         (null? (cddr ir)))))

(define atomic-non-var?
  (lambda (ir)
    (or (const? ir)
        (stamped? ir)
        (mactok? ir)
        (coretok? ir))))

(define atomic-not-stamped?
  (lambda (ir)
    (or (const? ir)
        (and (var? ir)
             (not (stamped? ir)))
        (mactok? ir)
        (coretok? ir))))

(define var? symbol?)

(define const?
  (lambda (x)
    (or (number? x) (string? x) (char? x))))

(define var cadr)
(define body caddr)
(define fun car)
(define arg cadr)


(put 'lambda 'coretok 'true)
(put 'quote 'coretok 'true)
(put 'let 'mactok 'true)
(put 'if 'mactok 'true)
(put 'or 'mactok 'true)
(put 'naive-or 'mactok 'true)
(put 'fake 'mactok 'true)
(put 'case 'mactok 'true)

(define-syntax record-case
  (let ()
    (define build-rc-body
      (lambda (p body)
        (syntax-case p ()
          ((id . p)
           (with-syntax ((body (build-rc-body (syntax p) body)))
             (syntax (let ((rec (cdr rec)))
                       (let ((id (car rec)))
                         body)))))
          (() (with-syntax ((body body))
                (syntax (begin . body))))
          (id
           (with-syntax ((body body))
             (syntax (let ((id (cdr rec))) . body)))))))

    (define-syntax build-clause
      (lambda (x)
        (syntax-case x ()
          ((_ tag keys idspec body rest)
           (syntax
             (with-syntax ((body (build-rc-body (syntax idspec) (syntax body))))
               (syntax (if (memv tag 'keys)
                           body
                           . rest))))))))
    (lambda (x)
      (syntax-case x ()
        ((_ e m1 m2 ...)
         (with-syntax
           ((body (let f ((clause (syntax m1)) (clauses (syntax (m2 ...))))
                    (if (null? clauses)
                        (syntax-case clause (else)
                          ((else e1 e2 ...) (syntax (begin e1 e2 ...)))
                          (((key ...) idspec e1 e2 ...)
                           (build-clause tag (key ...) idspec (e1 e2 ...) ()))
                          ((key idspec e1 e2 ...)
                           (build-clause tag (key) idspec (e1 e2 ...) ()))
                          (_ (syntax-error x)))
                        (with-syntax ((rest (f (car clauses) (cdr clauses))))
                          (syntax-case clause (else)
                            (((key ...) idspec e1 e2 ...)
                             (build-clause tag (key ...) idspec (e1 e2 ...)
                               (rest)))
                            ((key idspec e1 e2 ...)
                             (build-clause tag (key) idspec (e1 e2 ...) (rest)))
                            (_ (syntax-error x))))))))
           (syntax (let ((rec e))
                     (let ((tag (car rec)))
                       body)))))))))

(define macro?
  (lambda (ir)
    (record-case ir
      [let (var val body) true]
      [if (a b c) true]
      [or (a b) true]
      [naive-or (a b) true]
      [fake (x) true]
      [case (a b) true]
      [else false])))

(define syntax-table
  (lambda (ir)
    (record-case ir
      [let (i e b) `((lambda ,i ,b) ,e)]
      [if (a b c) `(((ef ,a) ,b) ,c)]
      [or (a b) `(let v ,a (if v v ,b))]
      [naive-or (a b)
        (let ([v (S0 'v)])
          `(let ,v ,a (if ,v ,v ,b)))]
      [fake (x) `(quote ,x)]
      [case (e pair)
       `(let v ,e
          (if ((eq? v) (quote ,(car pair)))
              ,(cadr pair)
              false))]
      [else (error 'syntax-table "syntax table: no match" ir)])))

;;; helper utilities

;;; demonstration
((exp '(let x (or a v) (naive-or x v))) syntax-table)
(printf "===============\n")
((exp '(lambda a (case (fake a) (quote a)))) syntax-table)

