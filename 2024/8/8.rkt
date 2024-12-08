#!/usr/bin/racket
#lang racket

(define lines (file->lines "input"))
(define testlines (file->lines "test_input"))

;; Use matrix-like hashes from problem 4
(define (lines->hash lines)
  (local ((define (aline->hash aline res c)
            (cond
              [(empty? aline) res]
              [else (aline->hash (rest aline) (hash-set res c (first aline)) (+ c 1))]))
          (define (lines->hash lines res c)
            (cond
              [(empty? lines) res]
              [(lines->hash (rest lines) (hash-set res c (aline->hash (map string (string->list (first lines))) (make-immutable-hash) 0)) (+ c 1))])))
    (lines->hash lines (make-immutable-hash) 0)))

(define testmat (lines->hash testlines))
(define mat (lines->hash lines))

(define (make-pair-hash amat)
  (local ((define (make-pair-hash amat xpos ypos hash-of-lists)
            (cond
              [(not (hash-ref amat ypos false))
               hash-of-lists]
              [(not (hash-ref (hash-ref amat ypos) xpos false))
               (make-pair-hash amat 0 (+ ypos 1) hash-of-lists)]
              [(string=? (hash-ref (hash-ref amat ypos) xpos) ".")
               (make-pair-hash amat (+ xpos 1) ypos hash-of-lists)]
              [else
               (let ([val (hash-ref (hash-ref amat ypos) xpos)])
                 (make-pair-hash amat (+ xpos 1) ypos
                                  (hash-set hash-of-lists val
                                            (append (hash-ref hash-of-lists val empty) (list (list xpos ypos))))))])))
    (make-pair-hash amat 0 0 (make-immutable-hash))))

(define (set-value amat x y val)
  (cond
    [(or (not (hash-ref amat y false))
         (not (hash-ref (hash-ref amat y) x false)))
     amat]
    [else (hash-set amat y (hash-set (hash-ref amat y) x val))]))

(define (add-antinodes amat)
  (local ((define (get-antinode-positions x1 y1 x2 y2)
            (let* ([xdist (- x1 x2)]
                   [ydist (- y1 y2)])
              (list (list (+ x1 xdist) (+ y1 ydist))
                    (list (- x2 xdist) (- y2 ydist)))))
          (define (add-antinodes-pair amat list-of-posn)
            (cond
              [(empty? list-of-posn) amat]
              [else (add-antinodes-pair (set-value amat (first (first list-of-posn))
                                                   (first (rest (first list-of-posn)))
                                                   "#")
                                        (rest list-of-posn))]))
          (define (add-pairs p1 alist amat)
            (cond
              [(empty? alist) amat]
              [else (add-pairs p1 (rest alist)
                               (add-antinodes-pair amat
                                                   (get-antinode-positions (first p1) (first (rest p1))
                                                                           (first (first alist)) (first (rest (first alist))))))]))
          (define (add-all-pairs alist amat)
            (cond
              [(or (empty? alist)
                   (empty? (rest alist))) amat]
              [else (add-all-pairs (rest alist) (add-pairs (first alist) (rest alist) amat))]))
          (define (add-all-antinodes pair-hash keys amat)
            (cond
              [(empty? keys) amat]
              [else (add-all-antinodes pair-hash (rest keys)
                                       (add-all-pairs (hash-ref pair-hash (first keys)) amat))])))
    (let* ([pair-hash (make-pair-hash amat)]
           [keys (hash-keys pair-hash)])
      (add-all-antinodes pair-hash keys amat))))

(define (count-antinodes amat)
  (local ((define (count-antinodes amat x y)
            (cond
              [(not (hash-ref amat y false)) 0]
              [(not (hash-ref (hash-ref amat y) x false))
               (count-antinodes amat 0 (+ y 1))]
              [(string=? (hash-ref (hash-ref amat y) x) "#")
               (+ 1 (count-antinodes amat (+ x 1) y))]
              [else
               (count-antinodes amat (+ x 1) y)])))
    (let ([antinode-mat (add-antinodes amat)])
      (count-antinodes antinode-mat 0 0))))

(define testanswer (count-antinodes testmat))
(define answer (count-antinodes mat))

;; Part two
;; we need to divide out all common factors of the distances between two antennas to get the unit step size
(define (factorize anum)
  (local ((define (factorize anum c)
            (cond
              [(= 1 anum) empty]
              [(> c (sqrt anum)) (list anum)]
              [(= (modulo anum c) 0)
               (cons c (factorize (/ anum c) c))]
              [else (factorize anum (+ c 1))])))
    (factorize anum 2)))


(define (get-antinode-positions-2 x1 y1 x2 y2 amat)
  (local ((define (list-subtract sorted-list1 sorted-list2)
            (cond
              [(empty? sorted-list1) empty]
              [(empty? sorted-list2) sorted-list1]
              [(= (first sorted-list1) (first sorted-list2))
               (list-subtract (rest sorted-list1) (rest sorted-list2))]
              [(> (first sorted-list2) (first sorted-list1))
               (cons (first sorted-list1) (list-subtract (rest sorted-list1) sorted-list2))]
              [(< (first sorted-list2) (first sorted-list1))
               (list-subtract sorted-list1 (rest sorted-list2))]))
          (define (list-intersect sorted-list1 sorted-list2)
            (cond
              [(or (empty? sorted-list1) (empty? sorted-list2)) empty]
              [(= (first sorted-list1) (first sorted-list2))
               (cons (first sorted-list1) (list-intersect (rest sorted-list1) (rest sorted-list2)))]
              [(> (first sorted-list1) (first sorted-list2))
               (list-intersect sorted-list1 (rest sorted-list2))]
              [(< (first sorted-list1) (first sorted-list2))
               (list-intersect (rest sorted-list1) sorted-list2)]))
          (define (get-unit-distances x1 y1 x2 y2)
            (let* ([xdist (- x1 x2)]
                   [ydist (- y1 y2)]
                   [xsgn (sgn xdist)]
                   [ysgn (sgn ydist)]
                   [x-factors (factorize (abs xdist))]
                   [y-factors (factorize (abs ydist))]
                   [common-factors (list-intersect x-factors y-factors)])
              (list (* xsgn (foldr * 1 (list-subtract x-factors common-factors)))
                    (* ysgn (foldr * 1 (list-subtract y-factors common-factors))))))
          (define (get-starting-pos x y xdist ydist amat)
            (let ([newx (- x xdist)]
                  [newy (- y ydist)])
              (cond
                [(or (< newx 0)
                     (< newy 0)
                     (not (hash-ref amat newy false))
                     (not (hash-ref (hash-ref amat newy) newx false))) (list x y)]
                [else (get-starting-pos newx newy xdist ydist amat)])))
          (define (get-all-antinode-posn x y xdist ydist amat)
            (cond
              [(or (< x 0) (< y 0)
                   (not (hash-ref amat y false))
                   (not (hash-ref (hash-ref amat y) x false))) empty]
              [else (cons (list x y)
                          (get-all-antinode-posn (+ x xdist) (+ y ydist) xdist ydist amat))])))
    (let* ([distances (get-unit-distances x1 y1 x2 y2)]
           [starting-pos (get-starting-pos x1 y1 (first distances) (first (rest distances)) amat)])
      (get-all-antinode-posn (first starting-pos) (first (rest starting-pos)) (first distances) (first (rest distances)) amat))))

;; Now we can just reuse code from before
(define (add-antinodes-2 amat)
  (local ((define (add-antinodes-pair amat list-of-posn)
            (cond
              [(empty? list-of-posn) amat]
              [else (add-antinodes-pair (set-value amat (first (first list-of-posn))
                                                   (first (rest (first list-of-posn)))
                                                   "#")
                                        (rest list-of-posn))]))
          (define (add-pairs p1 alist amat)
            (cond
              [(empty? alist) amat]
              [else (add-pairs p1 (rest alist)
                               (add-antinodes-pair amat
                                                   (get-antinode-positions-2 (first p1) (first (rest p1))
                                                                             (first (first alist)) (first (rest (first alist))) amat)))]))
          (define (add-all-pairs alist amat)
            (cond
              [(or (empty? alist)
                   (empty? (rest alist))) amat]
              [else (add-all-pairs (rest alist) (add-pairs (first alist) (rest alist) amat))]))
          (define (add-all-antinodes pair-hash keys amat)
            (cond
              [(empty? keys) amat]
              [else (add-all-antinodes pair-hash (rest keys)
                                       (add-all-pairs (hash-ref pair-hash (first keys)) amat))])))
    (let* ([pair-hash (make-pair-hash amat)]
           [keys (hash-keys pair-hash)])
      (add-all-antinodes pair-hash keys amat))))


(define (count-antinodes-2 amat)
  (local ((define (count-antinodes2 amat x y)
            (cond
              [(not (hash-ref amat y false)) 0]
              [(not (hash-ref (hash-ref amat y) x false))
               (count-antinodes2 amat 0 (+ y 1))]
              [(string=? (hash-ref (hash-ref amat y) x) "#")
               (+ 1 (count-antinodes2 amat (+ x 1) y))]
              [else
               (count-antinodes2 amat (+ x 1) y)])))
    (let ([antinode-mat (add-antinodes-2 amat)])
      (count-antinodes2 antinode-mat 0 0))))

(define testanswer2 (count-antinodes-2 testmat))
(define answer2 (count-antinodes-2 mat))
