(use-modules (ice-9 format))

;;; -> fun (fun (node cont))
;;; f is (node gen) -> fun(getter)
(define fringe
  (lambda (tree)
    (let f ((node tree)
	    (c (lambda (getter) (getter '(exhausted) '()))))
      (lambda (getter)
	(if (atom? node)
	    (getter node c)
	    ((f (car node)
		(lambda (getter1)
		  ((f (cdr node) c) getter1)))
	     getter))))))

(define samefringe
  (lambda (tree1 tree2)
    (let compare ((f1 (fringe tree1))
		  (f2 (fringe tree2)))
      (f1 (lambda (n1 c1)
	    (f2 (lambda (n2 c2)
		  (format #t "$$ ~a, ~a~%" n1 n2)
		  (if (not (equal? n1 n2))
		      #f
		      (if (equal? n1 '(exhausted))
			  #t
			  (compare c1 c2))))))))))


(define atom?
  (lambda (x)
    (not (pair? x))))

(define print
  (lambda (x)
    (display x)
    (newline)))

(define t1 '((3 4) (5 6)))
(define t2 '((3 4) (5 6)))
(define t3 '((3 a 4) (5 6)))

(define f1 (fringe t1))

(print "=====")
(define test
  (lambda (x c)
    (print x)))

(f1 test)

(print "=====")
(define walk
  (lambda (x c)
    (if (equal? x '(exhausted))
	(print "end")
	(begin
	  (print x)
	  (c walk)))))

(f1 walk)
