#!/usr/bin/racket
#lang racket
(define lines (file->lines "input"))
(define testlines (file->lines "test_input"))

(define (line->game line)
  (let ([sl (string-split line " | ")])
    (list (map string->number (rest (rest (string-split (first sl) #rx"[ \t]+"))))
          (map string->number (string-split (first (rest sl)) #rx"[ \t]+")))))

;; now the easy answer is to just take the size of the union...
(define (score aline)
  (let* ([game (line->game aline)]
         [size-overlap (length (set-intersect (first game) (first (rest game))))])
    (cond
      [(= 0 size-overlap) 0]
      [else (expt 2 (- size-overlap 1))])))

(define test-answer (foldl + 0 (map score testlines)))
(define answer1 (foldl + 0 (map score lines)))

(define multiply-table (make-hash))

(define (process-line gamenr line multiplicity)
  (local ((define (update-mult multiplicity gamenr score)
            (cond
              [(= 0 score) multiplicity]
              [else (begin (hash-set! multiplicity (+ gamenr score) (+ (hash-ref multiplicity gamenr 1)
                                                                       (hash-ref multiplicity (+ gamenr score) 1)))
                           (update-mult multiplicity gamenr (- score 1)))])))
    (let* ([game (line->game line)]
           [score (length (set-intersect (first game) (first (rest game))))])
      (update-mult multiplicity gamenr score))))

(define (process-games lines resulthash c)
  (cond
    [(empty? lines) resulthash]
    [else (process-games (rest lines) (process-line c (first lines) resulthash) (+ c 1))]))

(define multiplicity-result (process-games lines (make-hash) 1))
(define (total-scratchcards multhash)
  (local ((define (total multhash max-gamenr c)
            (cond
              [(> c max-gamenr) 0]
              [else (+ (hash-ref multhash c 1)
                       (total multhash max-gamenr (+ c 1)))])))
    (total multhash 215 1)))

(define answer2 (total-scratchcards (process-games lines (make-hash) 1)))
