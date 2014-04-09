#lang racket/gui

(define x-inc 50)
(define y-inc 60)

(define box-wid 18)
(define box-ht 8)

(define gdc '())

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

(define frame (new frame%
                   [label "draw-box"]
                   [width 640]
                   [height 480]))

(define paint-callback
  (λ (canvas dc)
    (if (null? gdc)
        (set! gdc dc)
        '())
    (draw-box dc usr-lst)))

(define canvas (new canvas%
                    [parent frame]
                    [paint-callback
                     paint-callback]))

(define draw-box
  (λ (dc lst)
    (let ((x x-inc))
      (let f ((lst lst)
              (y (double y-inc)))
        (cond
          ((null? lst) (draw-text "∅" x y))
          ((atom? lst) (draw-text dc (to-string lst) x y) (set! x (+ x x-inc)))
          (else (for-each (λ (e)
                            (draw-rect dc x y)
                            (f e (+ y y-inc)))
                          lst)))))))

(define draw-rect
  (λ (dc x y)
    (let* ((x1 (- x (half box-wid)))
           (y1 (+ y box-ht))
           (x2 (+ x (half box-wid)))
           (y2 y))
      (send dc draw-rectangle (- x box-wid) (- y box-ht) (double box-wid) (double box-ht))
      (send dc draw-line x (- y box-ht) x (+ y box-ht))
      (arrow dc x1 y1 x1 (+ y y-inc))
      (arrow dc x2 y2 (+ x2 (- x-inc (/ (* 3 box-wid) 2))) y))))

(define arrow
  (λ (dc x1 y1 x2 y2)
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
  (λ (n)
    (* n 2)))

(define half
  (λ (n)
    (/ n 2)))

(define atom?
  (λ (x)
    (not (or (null? x)
             (pair? x)))))

(define usr-lst '(+ a b (* (frac 3 4) a4) a5 (+ a6 a7)))
(send frame show #t)

;(arrow 10 10 100 100)