#lang racket/gui

(define x-inc 36)
(define y-inc 60)

(struct node (str x y) #:mutable #:transparent)

(define average
  (λ lst
    (let ((n (length lst))
          (sum (apply + lst)))
      (/ sum n))))

(define to-string
  (λ (x)
    (cond
      ((symbol? x) (symbol->string x))
      ((number? x) (number->string x))
      (else "?"))))

(define draw-text
  (λ (dc str x y)
    (let-values (((wid ht desc spc)
                  (send dc get-text-extent str)))
      (send dc draw-text str (- x (/ wid 2)) y))))

(define locate
  (λ (tree)
    (let ((x 0))
      (let f ((tree tree)
              (y 0))
        (cond
          ((pair? tree)
           (let* ((x1 (+ x x-inc))
                  (child
                   (map (λ (e)
                          (f e (+ y y-inc)))
                        (cdr tree)))
                  (parent
                   (node (to-string (car tree))
                         (average x1 x)
                         y)))
             (cons parent child)))
          (else
           (set! x (+ x x-inc))
           (node (to-string tree) x y)))))))

(define get-field
  (λ (e fn)
    (cond
      ((pair? e)
       (get-field (car e) fn))
      (else (fn e)))))

(define get-x
  (λ (e)
    (get-field e node-x)))

(define get-y
  (λ (e)
    (get-field e node-y)))

(define draw-tree
  (λ (dc tree)
    (cond
      ((pair? tree)
       (let* ((fst (car tree))
              (rst (cdr tree))
              (px (node-x fst))
              (py (node-y fst)))
         (draw-text dc (node-str fst) px py)
         (for-each
          (λ (e)
            (draw-tree dc e)
            (send dc draw-line
                  px (+ py 15) (get-x e) (get-y e)))
          rst)))
      (else
       (draw-text
        dc
        (node-str tree)
        (node-x tree)
        (node-y tree))))))

(define frame (new frame%
                   [label "draw-tree"]
                   [width 640]
                   [height 480]))

(define canvas (new canvas%
                    [parent frame]
                    [paint-callback
                     (λ (c dc)
                       (draw-tree dc
                                  (locate test-tree)))]))

(define test-tree '(+ a b (* (frac 3 4) a4) a5 (+ a6 a7)))
(send frame show #t)