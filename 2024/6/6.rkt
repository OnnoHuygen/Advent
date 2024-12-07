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
(define (get-start amaze)
  (local ((define (get-start amaze xpos ypos)
            (cond
              ;[(not (hash-ref amaze ypos)) (print "no starting pos found...")]
              [(not (hash-ref (hash-ref amaze ypos) xpos false))
               (get-start amaze 0 (+ ypos 1))]
              [(string=? (hash-ref (hash-ref amaze ypos) xpos) "^")
               (list xpos ypos)]
              [else (get-start amaze (+ xpos 1) ypos)])))
    (get-start amaze 0 0)))

(define (get-newdirection direction)
  (cond
    [(string=? "n" direction) "e"]
    [(string=? "e" direction) "s"]
    [(string=? "s" direction) "w"]
    [(string=? "w" direction) "n"]))

(define (get-newpos xpos ypos direction)
  (cond
    [(string=? "n" direction) (list xpos (- ypos 1))]
    [(string=? "s" direction) (list xpos (+ ypos 1))]
    [(string=? "e" direction) (list (+ xpos 1) ypos)]
    [(string=? "w" direction) (list (- xpos 1) ypos)]))

(define (update-maze amaze xpos ypos newvalue)
  (hash-set amaze ypos (hash-set (hash-ref amaze ypos) xpos newvalue)))

(define (follow-maze amaze)
  (local ((define (follow-maze amaze xpos ypos direction)
            (let* ([newpos (get-newpos xpos ypos direction)]
                   [newx (first newpos)]
                   [newy (first (rest newpos))])
              (cond
                [(or (not (hash-ref amaze newy false))
                     (not (hash-ref (hash-ref amaze newy) newx false))) 0]
                [(string=? (hash-ref (hash-ref amaze newy) newx) ".")
                 (+ 1 (follow-maze (update-maze amaze newx newy "X")
                                   newx newy direction))]
                [(string=? (hash-ref (hash-ref amaze newy) newx) "X")
                 (follow-maze amaze newx newy direction)]
                [(string=? (hash-ref (hash-ref amaze newy) newx) "#")
                 (follow-maze amaze xpos ypos (get-newdirection direction))]))))
    (let* ([startdirection (get-start amaze)]
           [startx (first startdirection)]
           [starty (first (rest startdirection))])
      (+ 1 (follow-maze (update-maze amaze startx starty "X") startx starty "n")))))

(define testanswer (follow-maze testmat))
(define answer1 (follow-maze mat))

;; Part 2
;; Try to reduce the computing time a bit by only trying locations
;; that the guard actually visits
(define (get-visited-positions amaze)
  (local ((define (get-visited-pos amaze xpos ypos direction)
            (let* ([newpos (get-newpos xpos ypos direction)]
                   [newx (first newpos)]
                   [newy (first (rest newpos))])
              (cond
                [(or (not (hash-ref amaze newy false))
                     (not (hash-ref (hash-ref amaze newy) newx false))) empty]
                [(string=? (hash-ref (hash-ref amaze newy) newx) ".")
                 (cons (list newx newy) (get-visited-pos (update-maze amaze newx newy "X")
                                   newx newy direction))]
                [(or (string=? (hash-ref (hash-ref amaze newy) newx) "X")
                     (string=? (hash-ref (hash-ref amaze newy) newx) "XX"))
                 (get-visited-pos amaze newx newy direction)]
                [(string=? (hash-ref (hash-ref amaze newy) newx) "#")
                 (get-visited-pos amaze xpos ypos (get-newdirection direction))]))))
    (let* ([startdirection (get-start amaze)]
           [startx (first startdirection)]
           [starty (first (rest startdirection))])
      (get-visited-pos (update-maze amaze startx starty "XX") startx starty "n"))))

(define (is-loop? amaze)
  (local ((define (already-visited-with-direction? amaze xpos ypos direction)
            (cond
              [(> (length (regexp-match* (regexp direction) (hash-ref (hash-ref amaze ypos) xpos))) 0)
               true]
              [else false]))
          (define (is-loop? amaze xpos ypos direction)
            (let* ([newpos (get-newpos xpos ypos direction)]
                   [newx (first newpos)]
                   [newy (first (rest newpos))])
              (cond
                [(or (not (hash-ref amaze newy false))
                     (not (hash-ref (hash-ref amaze newy) newx false))) false] ;got out
                [(string=? (hash-ref (hash-ref amaze newy) newx) "#")
                 (is-loop? amaze xpos ypos (get-newdirection direction))]
                [(already-visited-with-direction? amaze newx newy direction) true]
                [else
                 (is-loop? (update-maze amaze newx newy
                                        (string-append (hash-ref (hash-ref amaze newy) newx) direction))
                           newx newy direction)]))))
    (let* ([startdirection (get-start amaze)]
           [startx (first startdirection)]
           [starty (first (rest startdirection))])
      (is-loop? (update-maze amaze startx starty ".n") startx starty "n"))))


(define answer2 (length (filter (lambda (spos) (is-loop? (update-maze mat (first spos) (first (rest spos)) "#")))
                                (get-visited-positions mat))))
