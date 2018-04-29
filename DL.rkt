#lang racket
(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Damerau Levenshtein distance  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-2d-vector r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (str-k str k)
  (let* ( [val (char->integer (string-ref str k))])
    (cond [(> val 96) (- val 97)]
          [(and (> val 64) (< val 91))  (- val 65)]
          [(and (> val 32)  (<  val 58)) (- val 7)]
          [else (begin (displayln val) (error "Special character not supported")) ]
          )))

(define (edit-distance a b)
  (define sigma 51) ;was confused about the value of sigma.Caps, numbers and some special characters included  
  (define db 0)
  (define cost 0)
  (define k 0)
  (define l 0)
  (define m (string-length a))
  (define n (string-length b))
  (define da (make-vector sigma 0))
  (define d (make-2d-vector (+ m 2) (+ n 2) 0))
  
  (define maxdist (+ m n))
  (begin
    (2d-vector-set! d 0 0 maxdist)
    (for ( [i (in-range 1 (+ m 2))])
      (begin (2d-vector-set! d i 0 maxdist) (2d-vector-set! d i 1 (- i 1))))
    (for ( [j (in-range 1 (+ n 2))]) 
      (begin (2d-vector-set! d 0 j maxdist) (2d-vector-set! d 1 j (- j 1))))
    (for ([i (in-range 2 (+ m 2))])
      (set! db 0)
      (for ([j (in-range 2 (+ n 2))])
        (set! k (vector-ref da (str-k b (- j 2))))
        (set! l db)
        (begin
          (cond [ (= (str-k a (- i 2)) (str-k b (- j 2)))
                  (begin (set! cost 0) (set! db (- j 1)))]
                [else  (set! cost 1)])
          (2d-vector-set! d  i  j
                          (min (+ (2d-vector-ref d (- i 1)  (- j 1)) cost)             ;Substitution
                               (+ (2d-vector-ref d i (- j 1)) 1) ;Insertion
                               (+ (2d-vector-ref d (- i 1)  j) 1) ;Deletion
                               (+ (2d-vector-ref d k l ) (- i 1) (- j 1) (- k) (- l) -1)))
          )
        )
      (vector-set! da (str-k a (- i 2)) (- i 1)))
   
    (2d-vector-ref d (+ m 1) (+ n 1)))
  )
;;;;;;;;;;;;;make the call to maker of dictree here
;;
;;
;;
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))