#lang racket
(require "DL.rkt")
(provide (all-defined-out))

(struct bknode (pdis value nodes) #:transparent)

(define bki (open-input-file "dic.txt"))


(define (add bktree word)
  (define x (edit-distance word (bknode-value bktree)))
  (cond [(nodes-present? x (bknode-nodes bktree))
         (bknode (bknode-pdis bktree) (bknode-value bktree) (map (lambda (subtree)
                                                                   (cond [(= (bknode-pdis subtree) x)
                                                                          (add subtree word)]
                                                                         [else subtree]))
                                                                 (bknode-nodes bktree)))]
        [else
         (bknode (bknode-pdis bktree) (bknode-value bktree)
                 (cons (bknode x word '()) (bknode-nodes bktree)))]))

(define (nodes-present? val bnodes)
  (cond [(null? bnodes) #f]
        [(= val (bknode-pdis (car bnodes))) #t]
        [else (nodes-present? val (cdr bnodes))]))
(define tol 3)

(define (find-matching-words word tree)
  (define dis (edit-distance word (bknode-value tree)))
  (cond [(<= dis tol)
         (cons (bknode-value tree) (append* (map (lambda (bkn)
                                                   (cond [(<= (edit-distance (bknode-value bkn) word)
                                                              (+ dis tol))
                                                          (find-matching-words word bkn)]
                                                         [else '()]))
                                                 (bknode-nodes tree))))]
        [else
         (append* (map (lambda (bkn)
                         (define temp (edit-distance (bknode-value bkn) (bknode-value tree)))
                         (cond [(and (<= temp (+ dis tol))
                                     (>= temp (- dis tol)))
                                (find-matching-words word bkn)]
                               [else '()]))
                       (bknode-nodes tree)))]))
