
(define const?
  (lambda (x)
    (cond
      [(number? x) #t]
      [(and (pair? x)
            (prim? (car x))
            (all const? (cdr x)))
        #t]
      [else #f])))

(define prim?
  (lambda (x)
    (memq x '(+ - * / %))))

(define all
  (lambda (fn lst)
    (if (null? lst) #t
        (and (fn (car lst))
             (all fn (cdr lst))))))

;;; how to calculate an expression?
;;; generate assembly code

(define all-regs '(%rcx %rdx %rdi %rsi %r8 %r9 %r10 %r11))
(define used-regs '())
(define free-regs all-regs)

#;(define calc
  (lambda (x a)
    (define find-reg
      (lambda ()
        (if (null? free-regs)
            (let ([x (last used-regs)])
              (save x)
              x)
            (let ([x (car free-regs)])
              (set! free-regs (cdr free-regs))
              (set! used-regs (cons x used-regs))
              x))))
    (cond
      [(number? x)
       (printf "\tmovq $~s, ~s\n" x a)]
      [(and (pair? x)
            (prim? (car x)))
       (begin
         (calc (list-ref x 1) a)
         (let ([x1 (find-reg)])
           (calc (list-ref x 2) x1)
           (case (car x)
             ((+) (printf "\taddq ~s, ~s\n" x1 a))
             ((-) (printf "\tsubq ~s, ~s\n" x1 a))
             )
           (set! free-regs (cons x1 free-regs))))
       ])))
       

(define calc
  (lambda (x)
    (cond
      [(number? x)
       (emit 'movq x '%rax)]
      [else
       (let ([op (car x)]
             [x1 (list-ref x 1)]
             [x2 (list-ref x 2)])
         (calc x1)
         (emit 'pushq '%rax)
         (calc x2)
         (emit 'popq '%rcx)
         (case op
           ((+) (emit 'addq '%rcx '%rax))
           ((-) (emit 'subq '%rcx '%rax))
           ((*) (emit 'imulq '%rcx))
           ((/) (begin
                  (emit 'xchgq '%rcx '%rax)
                  (emit 'idivq '%rcx)))
           ((%) (begin
                  (emit 'xchgq '%rcx '%rax)
                  (emit 'idivq '%rcx)
                  (emit 'movq '%rdx %rax)))))])))

(define emit
  (case-lambda 
    [(op x1 x2)
     (printf "\t~s " op)
     (if (number? x1)
         (printf "$~s," x1)
         (printf "~s," x1))
     (if (number? x2)
         (printf "$~s\n" x2)
         (printf "~s\n" x2))]
    [(op x)
     (printf "\t~s ~s\n" op x)]))

