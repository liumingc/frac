#lang racket/gui

(define x-inc 50)
(define y-inc 50)

(define box-wid 18)
(define box-ht 8)

(define gdc '())

(struct node (txt x y) #:transparent)

(define to-string
  (lambda (x)
    (cond
      ((symbol? x) (symbol->string x))
      ((number? x) (number->string x))
      (else "?"))))

(define draw-text
  (lambda (dc str x y)
    (let-values (((wid ht desc spc)
                  (send dc get-text-extent str)))
      (send dc draw-text str (- x (/ wid 2)) y))))

(define loc
  (lambda (lst)
    (let ((x x-inc))
      (let rec ((lst lst)
                (y y-inc))
        (cond
          ((null? lst) '())
          ((atom? lst) (node (to-string lst) x y))
          (else (map (lambda (e)
                       (let* ((old-x x)
                              (n (node (rec e (+ y y-inc)) old-x y)))
                         (set! x (+ x x-inc))
                         n))
                    lst)))))))

(define frame (new frame%
                   [label "draw-box"]
                   [width 640]
                   [height 480]))

(define paint-callback
  (lambda (canvas dc)
    (if (null? gdc)
        (set! gdc dc)
        '())
    (draw dc usr-lst)))

(define canvas (new canvas%
                    [parent frame]
                    [paint-callback
                     paint-callback]))

(define fetch-node
  (lambda (e)
    (cond
      ((node? e) e)
      (else (fetch-node (car e))))))

(define draw
  (lambda (dc e)
    (cond
      ((null? e) '())
      ((atom? e) (draw-text dc (node-txt e) (node-x e) (node-y e)))
      (else 
       (define draw-for-inter
         (lambda (e ne)
           (draw-rect dc e)
           (let* ((ele (fetch-node (node-txt e)))
                  (hd-x (- (node-x ele) (half box-wid)))
                  (tl-x (+ (node-x ele) (half box-wid))))
             (arrow dc hd-x (node-y e) hd-x (- (node-y ele) box-ht))
             (arrow dc tl-x (node-y e) (- (node-x ne) box-wid) (node-y e))
             (draw dc (node-txt e)))))
       (define draw-for-last
         (lambda (e)
           (draw-rect dc e)
           (draw-nil dc e)
           (let* ((ele (fetch-node (node-txt e)))
                  (hd-x (- (node-x ele) (half box-wid)))
                  (tl-x (+ (node-x ele) (half box-wid))))
             (arrow dc hd-x (node-y e) hd-x (- (node-y ele) box-ht))
             (draw dc (node-txt e)))))
       (@for-each draw-for-inter
                 draw-for-last
                      e)))))

;;; use ft for the last element
(define @for-each
  (lambda (fn ft lst)
    (cond
      ((null? lst) '())
      ((last? lst) (ft (car lst)))
      (else (cons (fn (car lst) (cadr lst))
                 (@for-each fn ft (cdr lst)))))))

(define last?
  (lambda (lst)
    (and (pair? lst)
        (null? (cdr lst)))))

(define draw-rect
  (lambda (dc e)
    (let ((x (node-x e))
          (y (node-y e)))
      (send dc draw-rectangle (- x box-wid) (- y box-ht) (double box-wid) (double box-ht))
      (send dc draw-line x (- y box-ht) x (+ y box-ht)))))

(define draw-nil
  (lambda (dc e)
    (let ((x1 (node-x e))
          (x2 (+ (node-x e) box-wid))
          (y1 (- (node-y e) box-ht))
          (y2 (+ (node-y e) box-ht)))
      (send dc draw-line x1 y1 x2 y2))))

(define arrow
  (lambda (dc x1 y1 x2 y2)
    (let* ((unit 8)
           (α (atan (- y2 y1) (- x2 x1)))
           (a1 (+ α (/ 3.14 6)))
           (a2 (- α (/ 3.14 6)))
           (x3 (* unit (cos a1)))
           (y3 (* unit (sin a1)))
           (x4 (* unit (cos a2)))
           (y4 (* unit (sin a2))))
      (send dc draw-line x1 y1 x2 y2)
      (send dc draw-line x2 y2 (- x2 x3) (- y2 y3))
      (send dc draw-line x2 y2 (- x2 x4) (- y2 y4)))))

(define double
  (lambda (n)
    (* n 2)))

(define half
  (lambda (n)
    (/ n 2)))

(define atom?
  (lambda (x)
    (not (or (null? x)
             (pair? x)))))

(define usr-lst (loc '(+ a b (* (frac 3 4) a4) a5 (+ a6 a7))))
;(define usr-lst (loc '(+ 3 4)))
(send frame show #t)

#|
(for ((a (range 0 6.28 0.2)))
    (arrow gdc
           300
           300
           (+ 300 (* 100 (cos a)))
           (+ 300 (* 100 (sin a)))))
|#