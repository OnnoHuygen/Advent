#!/usr/bin/racket
#lang racket

(define lines (file->lines "input"))
(define testlines (file->lines "test_input"))

;; Primary:
;; five of a kind
;; four of a kind
;; full house(= "J" (hand-one hand))
;; three of a kind
;; two pair
;; one pair
;; high card (do actually compare the card number???)


;; Secondary:
;; compare single cards left to right until tie is broken

(define carddict
  '#hash(("A" . "13")
         ("K" . "12")
         ("Q" . "11")
         ("J" . "10")
         ("T" . "09")
         ("9" . "08")
         ("8" . "07")
         ("7" . "06")
         ("6" . "05")
         ("5" . "04")
         ("4" . "03")
         ("3" . "02")
         ("2" . "01")))

;; 13 different cards
(define-struct hand (one two three four five bid order1 order2))

(define replacements (list "A" "K" "Q" "T" "9" "8" "7" "6" "5" "4" "3" "2"))

(define (get-rank hand [cd carddict]) ;;-> hand
  (local ((define (second-order hand)
            (string->number
             (string-append
              (hash-ref cd (hand-one hand))
              (hash-ref cd (hand-two hand))
              (hash-ref cd (hand-three hand))
              (hash-ref cd (hand-four hand))
              (hash-ref cd (hand-five hand)))))
          (define (card->index acard) (string->number (hash-ref cd acard)))
          (define (get-count hand);; make it a hash?
            (let ([resulthash (make-hash)]
                  [i1 (card->index (hand-one hand))]
                  [i2 (card->index (hand-two hand))]
                  [i3 (card->index (hand-three hand))]
                  [i4 (card->index (hand-four hand))]
                  [i5 (card->index (hand-five hand))])
              (hash-set! resulthash i1 (+ (hash-ref resulthash i1 0) 1))
              (hash-set! resulthash i2 (+ (hash-ref resulthash i2 0) 1))
              (hash-set! resulthash i3 (+ (hash-ref resulthash i3 0) 1))
              (hash-set! resulthash i4 (+ (hash-ref resulthash i4 0) 1))
              (hash-set! resulthash i5 (+ (hash-ref resulthash i5 0) 1))
              resulthash))
          (define (two-of-kind? multlist encountered)
            (cond
              [(empty? multlist) false]
              [(and (= (first multlist) 2)
                    encountered) true]
              [(= (first multlist) 2) (two-of-kind? (rest multlist) true)]
              [else (two-of-kind? (rest multlist) encountered)]))
          (define (contains alist el)
            (cond
              [(empty? alist) false]
              [(equal? (first alist) el) true]
              [else (contains (rest alist) el)]))
          (define (first-order hand)
            (let ([multiplicities (hash-values (get-count hand))])
              (cond
                [(contains multiplicities 5) 26]
                [(contains multiplicities 4) 25]
                [(and (contains multiplicities 3)
                      (contains multiplicities 2)) 24]
                [(contains multiplicities 3) 23]
                [(two-of-kind? multiplicities false) 22]
                [(contains multiplicities 2) 21]
                [else 1]))));(foldl max (card->index (hand-one hand))
                        ;     (map card->index
                        ;          (list (hand-two hand)
                        ;                (hand-three hand)
                        ;                (hand-four hand)
                        ;                (hand-five hand))))]))))
    (make-hand (hand-one hand) (hand-two hand) (hand-three hand)
               (hand-four hand) (hand-five hand) (hand-bid hand)
               (first-order hand) (second-order hand))))


(define (line->hand line)
  (let* ([cb (string-split line " ")]
         [cards (map string (string->list (first cb)))]
         [bid (string->number (first (rest cb)))])
    (get-rank (make-hand (first cards) (first (rest cards)) (first (rest (rest cards)))
                         (first (rest (rest (rest cards)))) (first (rest (rest (rest (rest cards)))))
                         bid 0 0))))

(define (hand> h1 h2)
  (cond
    [(> (hand-order1 h1) (hand-order1 h2)) true]
    [(< (hand-order1 h1) (hand-order1 h2)) false]
    [(> (hand-order2 h1) (hand-order2 h2)) true]
    [(< (hand-order2 h1) (hand-order2 h2)) false]))
(define (qsort list-of-hands)
  (cond
    [(empty? list-of-hands) empty]
    [else (let ([piv (first list-of-hands)])
            (append (qsort (filter (lambda (h) (hand> piv h)) (rest list-of-hands)))
                    (list piv)
                    (qsort (filter (lambda (h) (hand> h piv)) (rest list-of-hands)))))]))

(define testhands (qsort (map line->hand testlines)))

(define hands-sorted (qsort (map line->hand lines)))

(define (get-score sorted-handlist)
  (local ((define (get-score sorted-handlist c)
            (cond
              [(empty? sorted-handlist) 0]
              [else (+ (* (hand-bid (first sorted-handlist)) c)
                       (get-score (rest sorted-handlist) (+ c 1)))])))
    (get-score sorted-handlist 1)))

(define testanswer (get-score testhands))
(define answer1 (get-score hands-sorted))


(define (print-handlist hlist)
  (cond
    [(empty? hlist) void]
    [else (begin
            (println (string-append
                    (hand-one (first hlist))
                    (hand-two (first hlist))
                    (hand-three (first hlist))
                    (hand-four (first hlist))
                    (hand-five (first hlist))
                    " "
                    (number->string (hand-bid (first hlist)))
                    " "
                    (number->string (hand-order1 (first hlist)))
                    " "
                    (number->string (hand-order2 (first hlist)))
                    ))
            (print-handlist (rest hlist)))]))

;; (print-handlist hands-sorted)
;; ---------------- Part two -----------------------


(define carddict2
  '#hash(("A" . "13")
         ("K" . "12")
         ("Q" . "11")
         ("J" . "00")
         ("T" . "09")
         ("9" . "08")
         ("8" . "07")
         ("7" . "06")
         ("6" . "05")
         ("5" . "04")
         ("4" . "03")
         ("3" . "02")
         ("2" . "01")))
;; Is a hand always strongest if you replace all jokers by the same card?
;; Or can there exist a hand where you need to make one joker a different card than another one?



(define (maximize-rank hand)
  (local ((define (check-replace card new-card)
            (cond
              [(string=? "J" card) new-card]
              [else card]))
          (define (replace-jokers hand new-card) ;; -> newhand with J replaced by newcard
            (make-hand (check-replace (hand-one hand) new-card)
                       (check-replace (hand-two hand) new-card)
                       (check-replace (hand-three hand) new-card)
                       (check-replace (hand-four hand) new-card)
                       (check-replace (hand-five hand) new-card)
                       (hand-bid hand)
                       (hand-order1 hand)
                       (hand-order2 hand)))
          (define (contains-jokers? hand)
            (cond
              [(or (string=? "J" (hand-one hand))
                   (string=? "J" (hand-two hand))
                   (string=? "J" (hand-three hand))
                   (string=? "J" (hand-four hand))
                   (string=? "J" (hand-five hand))) true]
              [else false]))
          (define (maximize-rank hand)
            (cond
              [(contains-jokers? hand)
               (first (reverse (qsort (map (lambda (c) (get-rank (replace-jokers hand c) carddict2))
                                           replacements))))]
              [else (get-rank hand)])))
    (maximize-rank hand)))


(define (line->hand2 line)
  (let* ([cb (string-split line " ")]
         [cards (map string (string->list (first cb)))]
         [bid (string->number (first (rest cb)))]
         [minhand (make-hand (first cards) (first (rest cards)) (first (rest (rest cards)))
                              (first (rest (rest (rest cards)))) (first (rest (rest (rest (rest cards)))))
                              bid 0 0)]
         [maxhand (maximize-rank (make-hand (first cards) (first (rest cards)) (first (rest (rest cards)))
                              (first (rest (rest (rest cards)))) (first (rest (rest (rest (rest cards)))))
                              bid 0 0))]
         [o1 (hand-order1 maxhand)]
         [o2 (hand-order2 (get-rank minhand carddict2))])
    
    (make-hand (first cards) (first (rest cards)) (first (rest (rest cards)))
                              (first (rest (rest (rest cards)))) (first (rest (rest (rest (rest cards)))))
                              bid o1 o2)))

(define testhands2 (qsort (map line->hand2 testlines))) ;; get score of maximized one, but keep jokers.
(define test2 (get-score testhands2))

(define hands2 (qsort (map line->hand2 lines)))
(define answer2 (get-score hands2))

(define (check-replace card new-card)
  (cond
    [(string=? "J" card) new-card]
    [else card]))
(define (replace-jokers hand new-card) ;; -> newhand with J replaced by newcard
  (make-hand (check-replace (hand-one hand) new-card)
             (check-replace (hand-two hand) new-card)
             (check-replace (hand-three hand) new-card)
             (check-replace (hand-four hand) new-card)
             (check-replace (hand-five hand) new-card)
             (hand-bid hand)
             (hand-order1 hand)
             (hand-order2 hand)))
(define (contains-jokers? hand)
  (cond
    [(or (string=? "J" (hand-one hand))
         (string=? "J" (hand-two hand))
         (string=? "J" (hand-three hand))
         (string=? "J" (hand-four hand))
         (string=? "J" (hand-five hand))) true]
              [else false]))
(define (maximize-rank-local hand)
  (cond
    [(contains-jokers? hand)
     (first (qsort (map (lambda (c) (get-rank (replace-jokers hand c) carddict2))
                        replacements)))]
    [else hand]))
