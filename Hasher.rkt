#lang racket

(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HASH FUNCTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require racket/performance-hint)
(require data/bit-vector)

(begin-encourage-inline
  (define (rotl32 x r)
    (get-in-range (bitwise-ior (arithmetic-shift x r)
                               (arithmetic-shift x (* -1 (- 32 r))))))

  (define (get-in-range x)
    (modulo x 10000000))
  
  (define (fmix32 h)
    (begin
      (set! h (bitwise-xor h (arithmetic-shift h -16)))
      (set! h (* h #x85ebca6b))
      (set! h (bitwise-xor h (arithmetic-shift h -13)))
      (set! h (* h #xc2b2ae35))
      (set! h (bitwise-xor h (arithmetic-shift h -16)))
      (get-in-range h))))


(define (murmur-hash key [seed 13])
  (let* [(len (string-length key))
         (data key)
         (nblocks (quotient len 4))
         (h1 seed)
         (c1 #xcc9e2d51)
         (c2 #x1b873593)
         (nblocksX4 (* 4 nblocks))
         (switch (bitwise-and len 3))
         (k1 0)]
    [begin
      [for ([i (in-range 0 nblocks)])
        [let* [(nblocksX4 (* 4 i))
               (k1 0)]
          [begin
            (for ([j (in-range 0 4)])
              [set! k1 (+ (char->integer (string-ref data (+ j nblocksX4))) (arithmetic-shift k1 8))])
            (set! k1 (* k1 c1))
            (set! k1 (rotl32 k1 15))
            (set! k1 (* k1 c2))
            (set! h1 (bitwise-xor h1 k1))
            (set! h1 (rotl32 h1 13))
            (set! h1 (get-in-range (+ #xe6546b64 (* h1 5))))]]]
      [set! k1 0]
      [cond
        [(= switch 3) (begin
                        (set! k1 (bitwise-xor k1 (arithmetic-shift (char->integer (string-ref data (+ nblocksX4 2))) 16)))
                        (set! k1 (bitwise-xor k1 (arithmetic-shift (char->integer (string-ref data (+ nblocksX4 1))) 8)))
                        (set! k1 (bitwise-xor k1 (char->integer (string-ref data nblocksX4))))
                        (set! k1 (* k1 c1))
                        (set! k1 (rotl32 k1 15))
                        (set! k1 (* k1 c2))
                        (set! h1 (get-in-range (bitwise-xor h1 k1))))]
        [(= switch 2) (begin
                        (set! k1 (bitwise-xor k1 (arithmetic-shift (char->integer (string-ref data (+ nblocksX4 1))) 8)))
                        (set! k1 (bitwise-xor k1 (char->integer (string-ref data nblocksX4))))
                        (set! k1 (* k1 c1))
                        (set! k1 (rotl32 k1 15))
                        (set! k1 (* k1 c2))
                        (set! h1 (get-in-range (bitwise-xor h1 k1))))]
        [(= switch 1) (begin
                        (set! k1 (bitwise-xor k1 (char->integer (string-ref data nblocksX4))))
                        (set! k1 (* k1 c1))
                        (set! k1 (rotl32 k1 15))
                        (set! k1 (* k1 c2))
                        (set! h1 (get-in-range (bitwise-xor h1 k1))))]
        [else 0]]
      [set! h1 (get-in-range (bitwise-xor h1 len))]
      h1]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;