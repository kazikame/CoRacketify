#lang racket
(require racket/gui/base)
(require "DL.rkt")
(require "Hasher.rkt")
(require "Bloom.rkt")
(require data/bit-vector)
(require net/sendurl)
(require "BKTree.rkt")

(define dictree (bknode 0 "correct" '()))

(define (make-tree)
  (define word (read-line bki))
  (cond [(not (eof-object? word))
         (set! dictree (add dictree word))
         (make-tree)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (space-check word)
  (let [(len (string-length word))
        (ans #f)]
    [begin (define (helper i)
             (if (= i len) (void)
                 (let [(part1 (substring word 0 i))
                       (part2 (substring word i len))]
                   [if (and (is-correct? part1) (is-correct? part2))
                       (set! ans (string-append part1 " " part2))
                       (helper (+ i 1))])))
           (helper 0) ans]))

(define (smart-correction word n)
  (let* ([temp (space-check word)]
         [x (find-matching-words word dictree)]
         [one-list (lc z : z <- x @(equal? (edit-distance z word) 1))]
         [l (length one-list)] 
         [res (if (>= l n) (first n one-list) (append one-list (first (- n l) x)))])
    (cond [(boolean? temp) res]
  
          [else (append (list temp) res)])
    ))
                                                        

(define (first n l)
  (cond [(or (null? l) (= n 0)) '()]
        [else (cons (car l) (first (- n 1) (cdr l)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (is-correct? word)

  (define new-word word)
  (is-correct-helper new-word 10))

(define (is-correct-helper word i)
  
  (cond [(= i 23) #t]
        [(equal? (bit-vector-ref bloom-vec (murmur-hash word i)) #f) #f ]
        [ else (is-correct-helper word (+ i 1))]
        ))
  


;;;;;;;;;Check Spellings;;;;;;;


(define (result-generator str)
  (define incorrect-list '())
  (define modified-string "")
  (define (readx)
    (let [(ans "")
          (len (string-length str))]
      [define (helper i)
        (cond [(= len 0) (set! ans "#eof")]
              [ (= i len) (begin (set! ans str) (set! str ""))]
              [else
               [let [(curr (string-ref str i))]
                 [cond
        
                   [(or (eq? #\space curr) (eq? #\newline curr))
                    (begin (set! ans (substring str 0 i)) (set! str (if (> len (+ i 1)) (substring str (+ i 1) len) "")))]
                   [(or (eq? #\. curr) (eq? #\? curr) (eq? #\, curr) (eq? #\! curr))
                    (begin (set! ans (substring str 0 i)) (set! str (if (> len (+ i 2)) (substring str (+ i 2) len) "")))]
                   [else (helper (+ i 1))]]]])]
      [begin (helper 0) ans]))
  (define (result-gen)
    (let* ([word (readx)])
      (cond [(equal? word "#eof") (cons modified-string incorrect-list )]
            [(equal? word "") (result-gen)]
            [else
             (if (not (is-correct? word)) (begin (set! incorrect-list (cons word incorrect-list))
                                                 (set! modified-string (string-append modified-string (string-append " " (modify word))))
                                                 (result-gen))
                 (begin
                   (set! modified-string (string-append modified-string (string-append " " word)))
                   (result-gen))
                 ) ]
            )))
  (result-gen)
  )


(define (print-solution str)
  (car (result-generator str)))

(define (incorrect-list str)
  (cdr (result-generator str)))

(define (modify word)
  (define wl (string->list word))
  (list->string (append (cons #\{ wl) '(#\})))
  )






(gen-vector)
(make-tree)




;;;;;;;;;;;;;;;;;;GUI;;;;;;;;;;;;;;;;;;;
(define sug #f)
(define replacement #f)

(define (find-and-replace str word1 word2)
  (define (readx)
    (let [(ans "")
          (len (string-length str))]
      [define (helper i)
        (cond [(= len 0) (set! ans "#eof")] 
              [ (= i len) (begin (set! ans str) (set! str ""))]
              [else
               [let [(curr (string-ref str i))]
                 [cond
        
                   [(or (eq? #\space curr) (eq? #\newline curr))
                    (begin (set! ans (substring str 0 i)) (set! str (if (> len (+ i 1)) (substring str (+ i 1) len) "")))]
                   [(or (eq? #\. curr) (eq? #\? curr) (eq? #\, curr) (eq? #\! curr))
                    (begin (set! ans (substring str 0 i)) (set! str (if (> len (+ i 2)) (substring str (+ i 2) len) "")))]
                   [else (helper (+ i 1))]]]])]
      [begin (helper 0) ans]))
  (define modified-string "")
  (define (fr)
    (let* ([word (readx)])
      (cond [(equal? word "#eof") (string-append " " modified-string)]
            [(equal? word "") (fr)]
            [else
             (if (equal? word word1) (begin 
                                       (set! modified-string (string-append modified-string (string-append " " word2)))
                                       (fr))
                 (begin
                   (set! modified-string (string-append modified-string (string-append " " word)))
                   (fr))
                 ) ]
            )))
  (let*
      [(strnew (fr))
       (lennew (string-length strnew))]

    [substring strnew 1 lennew]))


(define (func)
  (define temp (send input get-value))
  (define ans (print-solution temp))
  (send output set-value ans)
  (define inc (incorrect-list temp))
  (send incorrect-words update-choices inc))
  

(define mainframe (new frame%
                       [label "CorRacketify"]
                       [width 800]
                       [height 800]
                       [enabled #t]
                       [border 5]))

(send mainframe show #t)

(define panel1 (new horizontal-panel%
                    [parent mainframe]))

(define input (new text-field%
                   [parent panel1]
                   [label "Enter Your Text Here"]
                   [min-height 80]
                   [style (list 'multiple)]

                   [vert-margin 0]))
(define correct-button(new button%
                           [label "Correct It!"]
                           [parent panel1]
                           [callback (lambda (x y) (func))]
                           ))
(define panel2 (new horizontal-panel%
                    [parent mainframe]))

(define output (new text-field%
                    [parent panel2]
                    [label "Parsed Text"]
                    [min-height 60]
                    [horiz-margin 60]
                    [vert-margin 0]
                    [style (list 'multiple)]))

(define panel3 (new horizontal-panel%
                    [parent mainframe]))

(define incorrect-words 
  (new (class combo-field%
         (super-new)
         
         (inherit get-menu append)
         
         (define/public (update-choices choice-list)
           ; remove all the old items
           (map
            (lambda (i)
              (send i delete))
            (send (get-menu) get-items))
           ; set up the menu with all new items
           (map
            (lambda (choice-label)
              (append choice-label))
            choice-list)
           (void)
           ))
       [parent panel3]
       [label "Incorrect-Words"] 
       [choices '()]
       [callback (lambda (x y) (begin (send suggestions update-choices (smart-correction (send x get-value) 10))
                                      (set! sug (send x get-value))))]
       ))
(define add-button (new button%
                        [label "Add To Dictionary"]
                        [parent panel3]
                        [callback (lambda (x y) (map modify-vec (hasher sug))
                                    (set! dictree (add dictree sug)))]
                        ))

(define suggestions
  (new (class combo-field%
         (super-new)
         
         (inherit get-menu append)
         
         (define/public (update-choices choice-list)
           ; remove all the old items
           (map
            (lambda (i)
              (send i delete))
            (send (get-menu) get-items))
           ; set up the menu with all new items
           (map
            (lambda (choice-label)
              (append choice-label))
            choice-list)
           (void)
           ))
       [parent panel3]
       [label "Suggestions"] 
       [choices '()]
       [callback (lambda (x y) (set! replacement (send x get-value)))]
       
       ))

(define rep (new button%
                 [label "Replace"]
                 [parent panel3]
                 [callback (lambda (x y) (send input set-value (begin (find-and-replace (begin (send input get-value)) sug replacement))))]))

(define (google-search word)
  (define url  (string-append "https://www.google.com/search?q=" word "+meaning&oq=" word "+meaning&aqs=chrome..69i57j0l5.2108j0j8&sourceid=chrome&ie=UTF-8"))
  
  
  (send-url url ))

(define panel4 (new horizontal-panel%
                    [parent mainframe]))
(define search (new button%
                    [label "Get Meaning Of Suggestion"]
                    [parent panel4]
                    [callback (lambda (x y) (google-search replacement))]))



