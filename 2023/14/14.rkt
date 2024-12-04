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

(define testpattern (lines->hash testlines))
(define testpattern-transpose (lines->transpose-hash testlines))

(define pattern (lines->hash lines))
(define pattern-transpose (lines->transpose-hash lines))

(define (peel pattern)
  (local ((define (rocks-rolling-here arow index ymax)
            (cond
              [(or (> index ymax)
                   (string=? (hash-ref arow index) "#")) (list arow 0)]
              [(string=? (hash-ref arow index) "O") (list (hash-set arow index ".") 1)]
              [else (rocks-rolling-here arow (+ index 1) ymax)]))
          (define (peel-row arow index multiplier ymax)
            (cond
              [(> index ymax) 0]
              [else 
               (let* ([res (rocks-rolling-here arow index ymax)]
                      [newrow (first res)]
                      [rocks (first (rest res))])
                 (+ (* multiplier rocks)
                    (peel-row newrow (+ index 1) (- multiplier 1) ymax)))]))
          (define (peel pattern index)
            (cond
              [(> index (foldl max 0 (hash-keys pattern))) 0]
              [else (+ (peel-row (hash-ref pattern index) 0 (length (hash-keys (hash-ref pattern index)))
                                 (foldl max 0 (hash-keys (hash-ref pattern index))))
                       (peel pattern (+ index 1)))])))
    (peel pattern 0)))

(define testanswer1 (peel testpattern-transpose))
(define answer1 (peel pattern-transpose))

;; Part 2
;; This is unexpected, I thought you did not actually need to perform the cycling. Hence the above function that just computes.
;; Now it looks like we do, if only until a cycle is detected.
;; What if no cycle is detected? Need to do this one billion times.
;; Alright, write an efficient function then.

(define (move-up ahash)
  (local ((define (roll-here arow index xmax c) ;; returns row with rock moved up or not
            (cond
              [(or (> (+ index c) xmax)
                   (string=? (hash-ref arow (+ index c)) "#")) arow]
              [(string=? (hash-ref arow (+ index c)) "O")
               (hash-set (hash-set arow (+ index c) ".") index "O")];; remove and place
              [else (roll-here arow index xmax (+ c 1))]))
          (define (move-up-row row index xmax)
            (cond
              [(> index xmax) row]
              [(or (string=? (hash-ref row index) "O")
                   (string=? (hash-ref row index) "#")) (move-up-row row (+ index 1) xmax)]
              [else (move-up-row (roll-here row index xmax 1) (+ index 1) xmax)]))
          (define (move-up ahash index ymax xmax)
            (cond
              [(> index ymax) ahash]
              [else (move-up (hash-set ahash index (move-up-row (hash-ref ahash index) 0 xmax))
                             (+ index 1) ymax xmax)])))
    (move-up ahash 0 (foldl max 0 (hash-keys ahash))
             (foldl max 0 (hash-keys (hash-ref ahash 0))))))

(define (move-down ahash)
  (local ((define (roll-here arow index c) ;; returns row with rock moved down or not. 
            (cond
              [(or (< (- index c) 0)
                   (string=? (hash-ref arow (- index c)) "#")) arow]
              [(string=? (hash-ref arow (- index c)) "O")
               (hash-set (hash-set arow (- index c) ".") index "O")];; remove and place
              [else (roll-here arow index (+ c 1))]))
          (define (move-down-row row index)
            (cond
              [(< index 0) row]
              [(or (string=? (hash-ref row index) "O")
                   (string=? (hash-ref row index) "#")) (move-down-row row (- index 1))]
              [else (move-down-row (roll-here row index 1) (- index 1))]))
          (define (move-down ahash index ymax xmax)
            (cond
              [(> index ymax) ahash]
              [else (move-down (hash-set ahash index (move-down-row (hash-ref ahash index) xmax))
                               (+ index 1) ymax xmax)])))
    (move-down ahash 0 (foldl max 0 (hash-keys ahash))
               (foldl max 0 (hash-keys (hash-ref ahash 0))))))

(define (transpose ahash)
  (local ((define (get-row ahash x y ymax res)
            (cond
              [(> y ymax) res]
              [else (get-row ahash x (+ y 1) ymax
                             (hash-set res y (hash-ref (hash-ref ahash y) x)))]))
          (define (transpose ahash res index xmax ymax)
            (cond
              [(> index xmax) res]
              [else (transpose ahash
                               (hash-set res index (get-row ahash index 0 xmax (make-immutable-hash)))
                               (+ index 1)
                               xmax ymax)])))
    (let ([xmax (foldl max 0 (hash-keys (hash-ref ahash 0)))]
          [ymax (foldl max 0 (hash-keys ahash))])
      (transpose ahash (make-immutable-hash) 0 xmax ymax))))

(define (one-cycle tpat)
  (transpose (move-down (transpose (move-down (transpose (move-up (transpose (move-up tpat)))))))))

(define (n-cycles tpat n)
  (cond
    [(= 0 n) tpat]
    [(= 0 (modulo n 1000))
     (begin
       (display "only ")
       (display n)
       (display " cycles to go!")
       (display "\n")
       (n-cycles (one-cycle tpat) (- n 1)))]
    [else (n-cycles (one-cycle tpat) (- n 1))]))

;; This takes about 25s per 1000 cycles, thus needing a total of 289 days to finish.

;; So we need to detect cycles in the pattern.
;; first, we need a unique identifier. We only need to identify the locations of the rocks, the rest will stay the same.
;; Need one-to-one mapping of pattern -> int.
;; Then i can easily check

;; perhaps try pattern -> string first, easier.
(define (pattern->id pattern)
  (local ((define (row->string arow ind xmax)
            (cond
              [(> ind xmax) ""]
              [else (string-append (hash-ref arow ind)
                                   (row->string arow (+ ind 1) xmax))]))
          (define (pat->string pat ind xmax ymax)
            (cond
              [(> ind ymax) ""]
              [else (string-append (row->string (hash-ref pat ind xmax) 0 xmax)
                                   (pat->string pat (+ ind 1) xmax ymax))])))
    (pat->string pattern 0 (foldl max 0 (hash-keys (hash-ref pattern 0)))
                 (foldl max 0 (hash-keys pattern)))))

(define (get-cycle-properties pattern)
  (local ((define (get-cycle-properties pattern steps-taken history)
            (let ([id (pattern->id pattern)])
              (cond
                [(hash-has-key? history id) (list steps-taken (- steps-taken (hash-ref history id))
                                                  (hash-ref history id))];; done
                [else
                 (get-cycle-properties (one-cycle pattern) (+ steps-taken 1)
                                       (hash-set history id steps-taken))]))))
    (get-cycle-properties pattern 0 (make-immutable-hash))))

(define testcycleprops (get-cycle-properties testpattern-transpose))
(define cycleprops (get-cycle-properties pattern-transpose))

(define testresult
  (let ([burn-in (first (rest (rest testcycleprops)))]
        [cycle-length (first (rest testcycleprops))])
    (n-cycles (n-cycles testpattern-transpose burn-in) (modulo (- 1000000000 burn-in) cycle-length))))
(define result
  (let ([burn-in (first (rest (rest cycleprops)))]
        [cycle-length (first (rest cycleprops))])
    (n-cycles (n-cycles pattern-transpose burn-in) (modulo (- 1000000000 burn-in) cycle-length))))

(define (total-load pattern)
  (local ((define (total-row-load row multiplier)
            (* multiplier
               (length (filter (lambda (el) (string=? el "O")) (hash-values row)))))
          (define (total-load pattern index multiplier)
            (cond
              [(> index (foldl max 0 (hash-keys pattern)))
               0]
              [else (+ (total-row-load (hash-ref pattern index) multiplier)
                       (total-load pattern (+ index 1) (- multiplier 1)))])))
    (total-load pattern 0 (length (hash-keys pattern)))))

(define testanswer2 (total-load (transpose testresult)))
(define answer2 (total-load (transpose result)))
