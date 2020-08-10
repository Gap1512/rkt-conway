#lang racket

(require 2htdp/universe 2htdp/image)

(define (world-state width height nos)
  (list width height nos '() #t))

(define (render ws)
  (letrec ((size (/ (second ws) (third ws)))
           (cell (square size "solid" "black"))
           (rec (λ (lst result)
                  (cond
                    ((empty? lst) result)
                    (else (rec (cdr lst)
                            (place-image cell
                                         (* size (+ 0.5 (caar lst)))
                                         (* size (+ 0.5 (cadar lst)))
                                         result)))))))
    (rec (fourth ws) (rectangle (first ws) (second ws) "solid" "white"))))

(define (tock ws)
  (cond
    ((fifth ws) ws)
    (else (list-set ws 3 (rules (fourth ws))))))

(define (rules alives)
  (letrec ((rec (λ (lst result)
                  (cond
                    ((empty? lst) (remove-duplicates result))
                    (else (let ((cell (car lst)))
                            (rec (cdr lst) (append (clauses-alive (cons cell (count-alives (neighboors (first cell) (second cell)) (list 8 '())))) result)))))))
           (clauses-alive (λ (cell-info)
                            (let ((n (second cell-info)))
                              (append (if (or (= n 2) (= n 3)) (list (first cell-info)) '()) (clauses-dead (third cell-info) '())))))
           (clauses-dead (λ (cell-lst result)
                           (cond
                             ((empty? cell-lst) result)
                             (else
                              (let ((n (first (count-alives (neighboors (first (car cell-lst)) (second (car cell-lst))) (list 8 '())))))
                                (clauses-dead (cdr cell-lst) (if (= n 3) (cons (car cell-lst) result) result)))))))
           (neighboors (λ (x y)
                         (list (list (- x 1) (- y 1))
                               (list x (- y 1))
                               (list (+ x 1) (- y 1))
                               (list (- x 1) y)
                               (list (+ x 1) y)
                               (list (- x 1) (+ y 1))
                               (list x (+ y 1))
                               (list (+ x 1) (+ y 1)))))
           (count-alives (λ (lst acc)
                           (cond
                             ((empty? lst) acc)
                             (else (count-alives (cdr lst) (if (member (car lst) alives)
                                                               acc
                                                               (list (sub1 (first acc)) (cons (car lst) (second acc))))))))))
    (rec alives '())))

(define (live ws a-key)
  (cond   
    ((key=? a-key " ") (list-set ws 4 (not (fifth ws))))
    (else ws)))

(define (mark ws x y type)
  (let ((size (/ (second ws) (third ws))))
    (if (mouse=? type "button-up") (list-set ws 3 (cons (list (floor (/ x size)) (floor (/ y size))) (fourth ws))) ws)))

(define (main width height nos time)
  (big-bang (world-state width height nos)
    (on-tick tock time)
    (on-mouse mark)
    (to-draw render)
    (on-key live)))