#!/usr/bin/racket
#lang racket
(define lines (file->lines "input"))
(define testlines (file->lines "test_input"))

;; re-use some code
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

;; get content of tile
(define (get-tile amat x y)
  (hash-ref (hash-ref amat y) x))

(define testpattern (lines->hash testlines))

(define pattern (lines->hash lines))

;; Follow-line:
;; Follows line from top left going east until looped
;; Returns version of pattern where all locations of the beams are replaced by "#" and history of points visited
(define (follow-line pattern)
  (local ((define (step-self x y direction tile)
            (cond
              [(or (string=? tile ".")
                   (or (and (string=? tile "-")
                            (string=? direction "E"))
                       (and (string=? tile "-")
                            (string=? direction "W")))
                   (or (and (string=? tile "|")
                            (string=? direction "S"))
                       (and (string=? tile "|")
                            (string=? direction "N"))))
               (cond
                 [(string=? "E" direction) (list (+ x 1) y "E")]
                 [(string=? "N" direction) (list x (- y 1) "N")]
                 [(string=? "W" direction) (list (- x 1) y "W")]
                 [(string=? "S" direction) (list x (+ y 1) "S")])]
              [(string=? "/" tile)
               (cond
                 [(string=? "E" direction) (list x (- y 1) "N")]
                 [(string=? "N" direction) (list (+ x 1) y "E")]
                 [(string=? "W" direction) (list x (+ y 1) "S")]
                 [(string=? "S" direction) (list (- x 1) y "W")])]
              [(string=? "\\" tile);; 
               (cond
                 [(string=? "E" direction) (list x (+ y 1) "S")]
                 [(string=? "N" direction) (list (- x 1) y "W")]
                 [(string=? "W" direction) (list x (- y 1) "N")]
                 [(string=? "S" direction) (list (+ x 1) y "E")])]
              [else (list "Henkbert" "de" "benkhert")]))
          (define (to-key x y dir)
            (string-append (number->string x) "-" (number->string y) "-" dir))
          (define (follow-line pattern x y dir xmax ymax energized-pattern history)
            (cond
              [(or (> x xmax)
                   (> y ymax)
                   (< x 0)
                   (< y 0)
                   (hash-has-key? history (to-key x y dir))) (list energized-pattern history)]
              [else (let* ([t (get-tile pattern x y)]
                           [stp (step-self x y dir t)]
                           [newx (first stp)]
                           [newy (first (rest stp))]
                           [newdir (first (rest (rest stp)))])
                      (cond
                        [(or (string=? t ".")
                             (string=? t "/")
                             (string=? t "\\"))
                         (follow-line pattern newx newy newdir xmax ymax
                                      (hash-set energized-pattern y (hash-set (hash-ref energized-pattern y) x "#"))
                                      (hash-set history (to-key x y dir) 1))]
                        [(or (and (string=? t "|")
                                  (string=? dir "E"))
                             (and (string=? t "|")
                                  (string=? dir "W")))
                         (let ([res (follow-line pattern x (- y 1) "N" xmax ymax
                                                   (hash-set energized-pattern y (hash-set (hash-ref energized-pattern y) x "#"))
                                                   (hash-set history (to-key x y dir) 1))])
                           (follow-line pattern x (+ y 1) "S" xmax ymax (first res) (first (rest res))))]
                         ;; (follow-line pattern newx newy newdir xmax ymax
                         ;;                   (hash-set energized-pattern y (hash-set (hash-ref energized-pattern y) x "#")))]
                        [(or (and (string=? t "-")
                                  (string=? dir "S"))
                             (and (string=? t "-")
                                  (string=? dir "N")))

                         (let ([res (follow-line pattern (- x 1) y "W" xmax ymax
                                                   (hash-set energized-pattern y (hash-set (hash-ref energized-pattern y) x "#"))
                                                   (hash-set history (to-key x y dir) 1))])
                           (follow-line pattern (+ x 1) y "E" xmax ymax
                                        (first res)
                                        (first (rest res))))]
                        [else (follow-line pattern newx newy newdir xmax ymax
                                           (hash-set energized-pattern y (hash-set (hash-ref energized-pattern y) x "#"))
                                           (hash-set history (to-key x y dir) 1))]))])))
    (follow-line pattern 0 0 "E" (foldl max 0 (hash-keys (hash-ref pattern 0)))
                 (foldl max 0 (hash-keys pattern)) pattern (make-immutable-hash))))

;; Energize patterns
(define testpattern-energized (follow-line testpattern))
(define pattern-energized (follow-line pattern))
;; count "#" tiles
(define (energized-tiles pattern)
  (local ((define (energized-tiles-row arow)
            (length (filter (lambda (el) (string=? el "#")) (hash-values arow))))
          (define (energized-tiles pattern y)
            (cond
              [(= 0 y) (energized-tiles-row (hash-ref pattern y))]
              [else (+ (energized-tiles-row (hash-ref pattern y))
                       (energized-tiles pattern (- y 1)))])))
    (energized-tiles pattern (foldl max 0 (hash-keys pattern)))))

(define testanswer1 (energized-tiles (first testpattern-energized)))
(define answer1 (energized-tiles (first pattern-energized)))


;; Part two
;; Just pass initial directions as parameters instead of choosing top left east
(define (follow-line-from pattern x-init y-init dir-init)
  (local ((define (step-self x y direction tile)
            (cond
              [(or (string=? tile ".")
                   (or (and (string=? tile "-")
                            (string=? direction "E"))
                       (and (string=? tile "-")
                            (string=? direction "W")))
                   (or (and (string=? tile "|")
                            (string=? direction "S"))
                       (and (string=? tile "|")
                            (string=? direction "N"))))
               (cond
                 [(string=? "E" direction) (list (+ x 1) y "E")]
                 [(string=? "N" direction) (list x (- y 1) "N")]
                 [(string=? "W" direction) (list (- x 1) y "W")]
                 [(string=? "S" direction) (list x (+ y 1) "S")])]
              [(string=? "/" tile)
               (cond
                 [(string=? "E" direction) (list x (- y 1) "N")]
                 [(string=? "N" direction) (list (+ x 1) y "E")]
                 [(string=? "W" direction) (list x (+ y 1) "S")]
                 [(string=? "S" direction) (list (- x 1) y "W")])]
              [(string=? "\\" tile);; 
               (cond
                 [(string=? "E" direction) (list x (+ y 1) "S")]
                 [(string=? "N" direction) (list (- x 1) y "W")]
                 [(string=? "W" direction) (list x (- y 1) "N")]
                 [(string=? "S" direction) (list (+ x 1) y "E")])]
              [else (list "Henkbert" "de" "benkhert")]))
          (define (to-key x y dir)
            (string-append (number->string x) "-" (number->string y) "-" dir))
          (define (follow-line pattern x y dir xmax ymax energized-pattern history)
            (cond
              [(or (> x xmax)
                   (> y ymax)
                   (< x 0)
                   (< y 0)
                   (hash-has-key? history (to-key x y dir))) (list energized-pattern history)]
              [else (let* ([t (get-tile pattern x y)]
                           [stp (step-self x y dir t)]
                           [newx (first stp)]
                           [newy (first (rest stp))]
                           [newdir (first (rest (rest stp)))])
                      (cond
                        [(or (string=? t ".")
                             (string=? t "/")
                             (string=? t "\\"))
                         (follow-line pattern newx newy newdir xmax ymax
                                      (hash-set energized-pattern y (hash-set (hash-ref energized-pattern y) x "#"))
                                      (hash-set history (to-key x y dir) 1))]
                        [(or (and (string=? t "|")
                                  (string=? dir "E"))
                             (and (string=? t "|")
                                  (string=? dir "W")))
                         (let ([res (follow-line pattern x (- y 1) "N" xmax ymax
                                                   (hash-set energized-pattern y (hash-set (hash-ref energized-pattern y) x "#"))
                                                   (hash-set history (to-key x y dir) 1))])
                           (follow-line pattern x (+ y 1) "S" xmax ymax (first res) (first (rest res))))]
                         ;; (follow-line pattern newx newy newdir xmax ymax
                         ;;                   (hash-set energized-pattern y (hash-set (hash-ref energized-pattern y) x "#")))]
                        [(or (and (string=? t "-")
                                  (string=? dir "S"))
                             (and (string=? t "-")
                                  (string=? dir "N")))

                         (let ([res (follow-line pattern (- x 1) y "W" xmax ymax
                                                   (hash-set energized-pattern y (hash-set (hash-ref energized-pattern y) x "#"))
                                                   (hash-set history (to-key x y dir) 1))])
                           (follow-line pattern (+ x 1) y "E" xmax ymax
                                        (first res)
                                        (first (rest res))))]
                        [else (follow-line pattern newx newy newdir xmax ymax
                                           (hash-set energized-pattern y (hash-set (hash-ref energized-pattern y) x "#"))
                                           (hash-set history (to-key x y dir) 1))]))])))
    (follow-line pattern x-init y-init dir-init (foldl max 0 (hash-keys (hash-ref pattern 0)))
                 (foldl max 0 (hash-keys pattern)) pattern (make-immutable-hash))))

;; list of starting positions and directions
(define (starting-pos pattern)
  (append
   (map (lambda (el) (list el 0 "S")) (hash-keys (hash-ref pattern 0)))
   (map (lambda (el) (list el (foldl max 0 (hash-keys pattern)) "N")) (hash-keys (hash-ref pattern 0)))
   (map (lambda (el) (list 0 el "E")) (hash-keys pattern))
   (map (lambda (el) (list (foldl max 0 (hash-keys (hash-ref pattern 0))) el "W")) (hash-keys pattern))))

;; Maximize over all starting positions.
(define (get-answer pattern)
  (local ((define (get-answer pattern alist acc)
            (cond
              [(empty? alist) acc]
              [else (let* ([plist (first alist)]
                           [x (first plist)]
                           [y (first (rest plist))]
                           [dir (first (rest (rest plist)))]
                           [etiles (energized-tiles (first (follow-line-from pattern x y dir)))])
                      (get-answer pattern (rest alist) (max acc etiles)))])))
    (get-answer pattern (starting-pos pattern) 0)))

(define testanswer2 (get-answer testpattern))
(define answer2 (get-answer pattern))
