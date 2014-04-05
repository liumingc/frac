#lang racket/gui

(define draw-tree
  (λ (dc tree)
    (let ((x 0))
      (let dt ((tree tree)
               (y 0))
        (cond
          ((pair? tree)
           (let ((x1 x))
             (for-each
              (lambda (e)
                (dt e (+ y 50))
                (set! x (+ x 50)))
              (cdr tree))
             (let ((mid-x (/ (+ x1 (- x 50)) 2)))
               (send dc draw-text (car tree) mid-x y))))
          (else
           (send dc draw-text tree x y)
           (set! x (+ x 50))))))))

(define frame (new frame%
                   [label "draw-tree"]
                   [width 640]
                   [height 480]))

(define canvas (new canvas%
                    [parent frame]
                    [paint-callback
                     (λ (c dc)
                       (draw-tree dc test-tree))]))

(define test-tree '("a" "b" "c" "d" "e" ("+" "3" "4")))
(send frame show #t)