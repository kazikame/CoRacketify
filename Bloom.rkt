#lang racket
(provide (all-defined-out))
(require "Hasher.rkt")
(require data/bit-vector)
(define i (open-input-file "dic.txt"))
(define bloom-vec (make-bit-vector 10000000 #f))
(define (gen-vector)
  (define word (read-line i))
  (cond [(not (eof-object? word))
         (begin
           ;(set! word (operate word))
           (define hash-list (hasher word))
           (map modify-vec hash-list)
           (gen-vector))]))
(define (modify-vec num)
  (bit-vector-set! bloom-vec num #t))
;(define (operate word)
;  (define demo-word (string->list word))
;  (set! demo-word (reverse demo-word))
;  (cond [(char=? (car demo-word) #\newline)
;         (set! demo-word (cdr demo-word))])
;  (list->string (reverse demo-word)))
;(define (reverse l)
;  (rev-h l '()))
;(define (rev-h l l1)
;  (cond [(null? l) l1]
;        [else (rev-h (cdr l) (cons (car l) l1))]))


(define (hasher word)
  (define key-list '())
  (for ([i (in-range 10 23)])
    (set! key-list (cons (remainder (abs (murmur-hash word i)) 10000000) key-list)))
  key-list
  )


