#!/usr/bin/racket
#lang racket
(define lines (file->lines "input"))
(define testlines (file->lines "test_input"))

(define (split-lines lines)
  (local ((define (split-lines lines current)
            (cond
              [(empty? lines) (list current)]
              [(string=? (first lines) "")
               (cons current (split-lines (rest lines) empty))]
              [else (split-lines (rest lines) (append current (list (first lines))))])))
    (split-lines lines empty)))

;; re-use some code
;; But we are going to work with immutable hashes from now on.
;; They provide the hash-set functionality, that returns a new hash with the variable changed.
;; Way easier.
(define (lines->hash lines)
  (local ((define (aline->hash aline c res)
            (cond
              [(empty? aline) res]
              [else (aline->hash (rest aline) (+ c 1) (hash-set res c (first aline)))]))
          
          (define (lines->hash lines res c)
            (cond
              [(empty? lines) res]
              [else
               (lines->hash (rest lines) (hash-set res c (aline->hash (first lines) 0 (make-immutable-hash))) (+ c 1))])))
    (lines->hash (map (lambda (el) (map string el)) (map string->list lines))
                 (make-immutable-hash) 0)))

(define (lines->transpose-hash lines)
  (local ((define (add-line res aline c linenr)
            (cond
              [(empty? aline) res]
              [else (add-line (hash-set res c (hash-set (hash-ref res c (make-immutable-hash)) linenr (first aline)))
                              (rest aline) (+ c 1) linenr)]))
          (define (add-lines lines res linenr)
            (cond
              [(empty? lines) res]
              [else (add-lines (rest lines) (add-line res (first lines) 0 linenr) (+ linenr 1))])))
    (add-lines (map (lambda (el) (map string el)) (map string->list lines))
               (make-immutable-hash) 0)))

(define (get-tile amat x y)
  (hash-ref (hash-ref amat y) x))

(define testpatterns (map lines->hash (split-lines testlines)))
(define testpatterns-transpose (map lines->transpose-hash (split-lines testlines)))

(define patterns (map lines->hash (split-lines lines)))
(define patterns-transpose (map lines->transpose-hash (split-lines lines)))

;; Now we just have to compare rows.
(define (get-reflection p)
  (local ((define (get-reflection pattern c)
            (cond
              [(> c (- (length (hash-keys pattern)) 1)) empty]
              [(and (equal? (hash-ref pattern c) (hash-ref pattern (- c 1)))
                    (proper-reflection? pattern c 0))
               (cons c (get-reflection pattern (+ c 1)))]
              [else (get-reflection pattern (+ c 1))]))
          (define (proper-reflection? pattern index c)
            (cond
              [(or (< (- index 1 c) 0)
                   (> (+ index c) (- (length (hash-keys pattern)) 1))) true]
              [(equal? (hash-ref pattern (- index 1 c))
                       (hash-ref pattern (+ c index)))
               (proper-reflection? pattern index (+ c 1))]
              [else false]))
          )
    (get-reflection p 1)))

(define (get-score p pt)
  (let ([cols (get-reflection pt)];; list of proper reflections (can be empty)
        [rows (get-reflection p)])
    (+ (foldl + 0 cols)
       (* 100 (foldl + 0 rows)))))

(define (get-answer plist ptlist)
  (cond
    [(empty? plist) 0]
    [else (+ (get-score (first plist) (first ptlist))
             (get-answer (rest plist) (rest ptlist)))]))

(define testanswer1 (get-answer testpatterns testpatterns-transpose))
(define answer1 (get-answer patterns patterns-transpose))


;; Part two
;; Hamming distance between two rows in the hash
(define (distance r1 r2)
  (local ((define (distance r1 r2 keys)
            (cond
              [(empty? keys) 0]
              [(equal? (hash-ref r1 (first keys))
                       (hash-ref r2 (first keys)))
               (distance r1 r2 (rest keys))]
              [else (+ 1 (distance r1 r2 (rest keys)))])))
    (distance r1 r2 (hash-keys r1))))

;; Small adaptation.
;; Iterate over columns, but now check if a column is the same or 1 hamming distance away from the previous column.
;; If this is the case, check if it is a proper reflection by expanding outward, keeping track of the total hamming distance accumulated.
;; Finally, if the end of the hash is reached and the hamming distance is exactly one, it is a proper reflection.
(define (get-smudged-reflection p)
  (local ((define (get-smudged-reflection pattern c)
            (cond
              [(> c (- (length (hash-keys pattern)) 1)) empty]
              [(and (or (equal? (hash-ref pattern c) (hash-ref pattern (- c 1)))
                        (= 1 (distance (hash-ref pattern c) (hash-ref pattern (- c 1)))))
                    (proper-smudged-reflection? pattern c 0 0))
               (cons c (get-smudged-reflection pattern (+ c 1)))]
              [else (get-smudged-reflection pattern (+ c 1))]))
          (define (proper-smudged-reflection? pattern index c cumdist)
            (cond
              [(and (or (< (- index 1 c) 0)
                        (> (+ index c) (- (length (hash-keys pattern)) 1)))
                    (= 1 cumdist)) true]
              [(or (< (- index 1 c) 0)
                   (> (+ index c) (- (length (hash-keys pattern)) 1))) false]
              [(> cumdist 1)false]
              [else (proper-smudged-reflection? pattern index (+ c 1) (+ cumdist (distance (hash-ref pattern (- index 1 c))
                                                                                           (hash-ref pattern (+ index c)))))])))
    (get-smudged-reflection p 1)))

(define (get-score2 p pt)
  (let ([cols (get-smudged-reflection pt)];; list of proper reflections (can be empty)
        [rows (get-smudged-reflection p)])
    (+ (foldl + 0 cols)
       (* 100 (foldl + 0 rows)))))

(define (get-answer2 plist ptlist)
  (cond
    [(empty? plist) 0]
    [else (+ (get-score2 (first plist) (first ptlist))
             (get-answer2 (rest plist) (rest ptlist)))]))

(define testanswer2 (get-answer2 testpatterns testpatterns-transpose))
(define answer2 (get-answer2 patterns patterns-transpose))
