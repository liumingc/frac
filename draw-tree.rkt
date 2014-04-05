#lang racket/gui

(define x-inc 36)
(define y-inc 60)

(struct node (str x y) #:mutable #:transparent)

(define average
  (λ lst
    (let ((n (length lst))
          (sum (apply + lst)))
      (/ sum n))))

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
                          (set! x (+ x x-inc))
                          (f e (+ y y-inc)))
                        (cdr tree)))
                  (parent
                   (node (symbol->string (car tree))
                         (average x1 x)
                         y)))
             (cons parent child)))
           (else
            (set! x (+ x x-inc))
            (node (symbol->string tree) x y)))))))

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
         (send dc draw-text (node-str fst) px py)
         (for-each
          (λ (e)
            (draw-tree dc e)
            (send dc draw-line
                  px (+ py 15) (get-x e) (get-y e)))
          rst)))
      (else
       (send dc draw-text
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

(define test-tree '(+ a b (* a3 a4) a5 (+ a6 a7)))
(send frame show #t)