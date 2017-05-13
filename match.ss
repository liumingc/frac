
#|
(match '(+ a b)
       [(,op ,x ,y) (list x y)]
       [(,op ,x) (list x 0)])

->

(let ([x '(+ a b)])
  (let ([ls/fail (sexp-dispatch x '(any any any))])
    (if ls/fail
      (apply (lambda (op x y)
	       (list x y)) ls/fail)
      (let ([ls/fail2 (sexp-dispatch x '(any any))])
	(if ls/fail2
	  (apply (lambda (op x)
		   (list x 0)) ls/fail2)
	  (error 'match "~s~%" x))))))

;;; TODO: support ellipse
|#

(define-syntax match
  (lambda (x)
    (syntax-case x ()
      [(_ x clause ...)
       #'(let ([t x])
	   (match1 t clause ...))])))

(define-syntax match1
  (lambda (x)
    (syntax-case x ()
      [(_ x)
       #'(syntax-error x)]
      [(_ x cls rest ...)
       (with-syntax ([rest #'(match1 x rest ...)])
	 (syntax-case #'cls ()
	   [(pat expr)
	    (let-values ([(p pvars) (convert-pattern #'pat)])
	      ;(printf "convert pattern output: pattern->~s pvars->~s~%" p pvars)
	      #`(let ([ls/fail (sexp-dispatch x '#,p)])
		  (if ls/fail
		    (apply (lambda #,pvars expr) ls/fail)
		    rest)))]))])))

(define convert-pattern
  (lambda (pat)
    (define cvt
      (lambda (pat ids)
	(syntax-case pat (unquote)
	  [,x (values #'any (cons #'x ids))]
	  [() (values '() ids)]
	  [(x . y)
	   (let-values ([(ypat yvars) (cvt #'y ids)])
	     (let-values ([(xpat xvars) (cvt #'x yvars)])
	       (values (cons xpat ypat) xvars)))]
	  [x (values (vector #'atom #'x) ids)])))
    (cvt pat '())))

(define sexp-dispatch
  (lambda (x pat)
    (define disp
      (lambda (x pat r)
	(cond
	  [(not r) #f]
	  [(null? pat) (and (null? x) r)]
	  [(eq? pat 'any) (cons x r)]
	  [(vector? pat) (and (equal? (vector-ref pat 1) x) r)]
	  [(pair? pat)
	   (and (pair? x)
		(disp (car x) (car pat) (disp (cdr x) (cdr pat) r)))])))
    (disp x pat '())))

#!eof


