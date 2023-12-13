#!/usr/bin/racket
#lang racket
(define lines (file->lines "input"))
(define testlines (file->lines "test_input"))


(define (lines->hash lines)
  (local ((define (aline->hash aline res c)
            (cond
              [(empty? aline) res]
              [else (begin
                      (hash-set! res c (first aline))
                      (aline->hash (rest aline) res (+ c 1)))]))
          (define (lines->hash lines res c)
            (cond
              [(empty? lines) res]
              [(begin
                 (hash-set! res c (aline->hash (map string (string->list (first lines))) (make-hash) 0))
                 (lines->hash (rest lines) res (+ c 1)))])))
    (lines->hash lines (make-hash) 0)))

(define (get-tile amat x y)
  (hash-ref (hash-ref amat y) x))

(define testmat (lines->hash testlines))
(define mat (lines->hash lines))

(define (get-starting-pos amat)
  (local ((define (get-starting-pos amat x y dimx dimy)
            (cond
              [(> x (- dimx 1)) (get-starting-pos amat 0 (+ y 1) dimx dimy)]
              [(string=? (get-tile amat x y) "S") (list x y)]
              [else (get-starting-pos amat (+ x 1) y dimx dimy)])))
    (get-starting-pos amat 0 0 (length (hash-keys (hash-ref amat 0))) (length (hash-keys amat)))))

(define (step amat poslist)
  (let* ([x (first poslist)]
         [y (first (rest poslist))]
         [direction (first (rest (rest poslist)))]
         [tile (get-tile amat x y)])
    (cond
      [(and (string=? tile "-")
            (string=? direction "E")) (list (+ x 1) y "E")]
      [(and (string=? tile "-")
            (string=? direction "W")) (list (- x 1) y "W")]
      [(and (string=? tile "|")
            (string=? direction "S")) (list x (+ y 1) "S")]
      [(and (string=? tile "|")
            (string=? direction "N")) (list x (- y 1) "N")]
      [(and (string=? tile "7")
            (string=? direction "E")) (list x (+ y 1) "S")]
      [(and (string=? tile "7")
            (string=? direction "N")) (list (- x 1) y "W")]
      [(and (string=? tile "J")
            (string=? direction "S")) (list (- x 1) y "W")]
      [(and (string=? tile "J")
            (string=? direction "E")) (list x (- y 1) "N")]
      [(and (string=? tile "L")
            (string=? direction "W")) (list x (- y 1) "N")]
      [(and (string=? tile "L")
            (string=? direction "S")) (list (+ x 1) y "E")]
      [(and (string=? tile "F")
            (string=? direction "N")) (list (+ x 1) y "E")]
      [(and (string=? tile "F")
            (string=? direction "W")) (list x (+ y 1) "S")])))

(define (valid-step? x y direction amat)
            (let ([tile (get-tile amat x y)])
              (cond
                [(and (string=? direction "E")
                      (or (string=? tile "-")
                          (string=? tile "J")
                          (string=? tile "7"))) true]
                [(and (string=? direction "W")
                      (or (string=? tile "-")
                          (string=? tile "F")
                        (string=? tile "L"))) true]
                [(and (string=? direction "S")
                      (or (string=? tile "|")
                          (string=? tile "J")
                          (string=? tile "L"))) true]
                [(and (string=? direction "N")
                      (or (string=? tile "|")
                          (string=? tile "F")
                          (string=? tile "7"))) true]
                [else false])))

(define (get-initial-directions amat x y)
            (filter (lambda (al) (valid-step? (first al) (first (rest al)) (first (rest (rest al))) amat))
                    (filter (lambda (al) (and (>= (first al) 0) (>= (first (rest al)) 0)))
                            (list (list (+ x 1) y "E") (list (- x 1) y "W") (list x (+ y 1) "S") (list x (- y 1) "N")))))

(define (get-steps amat)
  (local ((define (get-steps-l amat poslist1 poslist2 steps-taken)
            (cond
              [(and (= (first poslist1) (first poslist2))
                    (= (first (rest poslist1)) (first (rest poslist2)))) steps-taken]
              [else (get-steps-l amat (step amat poslist1) (step amat poslist2) (+ steps-taken 1))])))
    (let* ([startpos (get-starting-pos amat)]
           [poslistlist (get-initial-directions amat (first startpos) (first (rest startpos)))]
           [pos1 (first poslistlist)]
           [pos2 (first (rest poslistlist))])
      (get-steps-l amat pos1 pos2 1))))

(define testanswer (get-steps testmat))
(define answer (get-steps mat))

;; Part two
;; Should actually be easy.
;; We walk the loop. As we walk the loop, we try to replace all tiles to the right hand side of the direction we are walking in with an O.
;; Once we have done this, we propagate all O's (i.e. an O makes all neighboring "." into an O).
;; No wait.. the non-loops are not simple "."s

;; But we can easily replace them by "."s by following the loop once and keeping track of the points visited, then set all others to "."

(define (clockwise-loop amat)
  (local ((define (get-loop-points amat pos points-visited)
            (cond
              [(string=? (get-tile amat (first pos) (first (rest pos))) "S")
               (append points-visited (list pos))]
              [else (get-loop-points amat (step amat pos) (append points-visited (list pos)))])))
    (let* ([startpos (get-starting-pos amat)]
           [startdir (get-initial-directions amat (first startpos) (first (rest startpos)))])
      (get-loop-points amat (step amat (first startdir)) (list (first startdir))))))

(define (counterclockwise-loop amat)
  (local ((define (get-loop-points amat pos points-visited)
            (cond
              [(string=? (get-tile amat (first pos) (first (rest pos))) "S")
               (append points-visited (list pos))]
              [else (get-loop-points amat (step amat pos) (append points-visited (list pos)))])))
    (let* ([startpos (get-starting-pos amat)]
           [startdir (get-initial-directions amat (first startpos) (first (rest startpos)))])
      (get-loop-points amat (step amat (first (rest startdir))) (list (first (rest startdir)))))))


(define (make-dothash amat)
  (local ((define (make-dotline x xmax res)
            (cond
              [(> x xmax) res]
              [else (begin
                      (hash-set! res x ".")
                      (make-dotline (+ x 1) xmax res))]))
          (define (make-dothash y xmax ymax res)
            (cond
              [(> y ymax) res]
              [else (begin
                      (hash-set! res y (make-dotline 0 xmax (make-hash)))
                      (make-dothash (+ y 1) xmax ymax res))])))
    (make-dothash 0 (length (hash-keys (hash-ref amat 0))) (length (hash-keys amat)) (make-hash))))

(define (dottify p-removed-hash loop-points);; create similar with X part of the loop and 
  (cond
    [(empty? loop-points) p-removed-hash]
    [else
     (let ([x (first (first loop-points))]
           [y (first (rest (first loop-points)))])
       (begin
         (hash-set! (hash-ref p-removed-hash y) x "X")
         (dottify p-removed-hash (rest loop-points))))]))

(define (get-right-point amat pos)
  (let ([x (first pos)]
        [y (first (rest pos))]
        [dir (first (rest (rest pos)))])
    (cond
      [(string=? dir "N") (list (+ x 1) y)]
      [(string=? dir "E") (list x (+ y 1))]
      [(string=? dir "S") (list (- x 1) y)]
      [(string=? dir "W") (list x (- y 1))])))


(define (get-left-point amat pos)
  (let ([x (first pos)]
        [y (first (rest pos))]
        [dir (first (rest (rest pos)))])
    (cond
      [(string=? dir "N") (list (- x 1) y)]
      [(string=? dir "E") (list x (- y 1))]
      [(string=? dir "S") (list (+ x 1) y)]
      [(string=? dir "W") (list x (+ y 1))])))

(define (try-replace-neighbors amat pos) ;; checks if pos is valid, checks if mat contains "." at location, then replaces by "I".
  (cond
    [(or (> (first pos) (- (length (hash-keys (hash-ref amat 0))) 1))
         (> (first (rest pos)) (- (length (hash-keys (hash-ref amat 0))) 1))
         (< (first pos) 0)
         (< (first (rest pos)) 0)) amat]
    [(string=? (get-tile amat (first pos) (first (rest pos))) "X")
     amat]
    [else (begin
            (hash-set! (hash-ref amat (first (rest pos))) (first pos) "I")
            amat)]))

(define (simplify-mat amat)
  (local ((define (run-replace-I dothash clockwise-list counter-list)
            (cond
              [(empty? clockwise-list) dothash]
              [else (let ([right-point (get-left-point dothash (first clockwise-list))]
                          [left-point (get-right-point dothash (first counter-list))])
                      (run-replace-I (try-replace-neighbors (try-replace-neighbors dothash right-point) left-point)
                                     (rest clockwise-list) (rest counter-list)))])))
    (let ([clockwise-list (clockwise-loop amat)]
          [counter-list (counterclockwise-loop amat)]
          [dothash (make-dothash amat)])
      (run-replace-I (dottify dothash clockwise-list) clockwise-list counter-list))))

(define (get-all-Is amat)
  (local ((define (get-all-Is amat x y)
            (cond
              [(> y (- (length (hash-keys amat)) 1)) empty]
              [(> x (- (length (hash-keys (hash-ref amat 0))) 1))
               (get-all-Is amat 0 (+ y 1))]
              [(string=? (get-tile amat x y) "I")
               (cons (list x y) (get-all-Is amat (+ x 1) y))]
              [else (get-all-Is amat (+ x 1) y)])))
    (get-all-Is amat 0 0)))

(define (replace-in-mat amat pos newval)
  (begin
    (hash-set! (hash-ref amat (first (rest pos))) (first pos) newval)
    amat))

(define (propagate-I simplified)
  (local ((define (is-valid-pos pos amat)
            (cond
              [(or (< (first pos) 0)
                   (< (first (rest pos)) 0)
                   (> (first pos) (- (length (hash-keys (hash-ref amat 0))) 1))
                   (> (first (rest pos)) (- (length (hash-keys amat)) 1))) false]
               [else true]))
          (define (get-neighbor-dots pos amat) ;; get all neighbor dot points.
            (let ([x (first pos)]
                  [y (first (rest pos))])
              (filter (lambda (p) (string=? "." (get-tile amat (first p) (first (rest p)))))
                      (filter (lambda (p) (is-valid-pos p amat)) (list (list (+ x 1) y) (list (- x 1) y)
                                                                       (list x (- y 1)) (list x (+ y 1)))))))
          (define (propagate-Is Ilist reshash)
            (cond
              [(empty? Ilist) reshash]
              [else (begin
                      (hash-set! (hash-ref reshash (first (rest (first Ilist)))) (first (first Ilist)) "I")
                      (propagate-Is (append (rest Ilist) (get-neighbor-dots (first Ilist) reshash)) reshash))])))
    (propagate-Is (get-all-Is simplified) simplified)))
    

(define dh (propagate-I (simplify-mat mat)))
(define answer2 (length (get-all-Is dh)))
