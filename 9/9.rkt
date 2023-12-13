#!/usr/bin/racket
#lang racket

(define lines (file->lines "input"))
(define testlines (file->lines "test_input"))


(define (extrapolate aline [side "r"])
  (local ((define (diffline aline prev)
            (cond
              [(empty? aline) empty]
              [else (cons (- (first aline) prev)
                          (diffline (rest aline) (first aline)))]))
          (define (all=? alist entry)
            (cond
              [(empty? alist) true]
              [(equal? (first alist) entry)
               (all=? (rest alist) entry)]
              [else false]))
          (define (to-hash current-line c result)
            (cond
              [(all=? current-line 0) result]
              [else (let ([dlist (diffline (rest current-line) (first current-line))])
                      (begin
                        (hash-set! result (+ c 1) dlist)
                        (to-hash dlist (+ c 1) result)))]))
          (define (extrapolate-r ahash c difval)
            (cond
              [(= 0 c) (+ (last (hash-ref ahash c)) difval)]
              [else (extrapolate-r ahash (- c 1) (+ (last (hash-ref ahash c)) difval))]))
          (define (extrapolate-l ahash c difval)
            (cond
              [(= 0 c) (- (first (hash-ref ahash c)) difval)]
              [else (extrapolate-l ahash (- c 1) (- (first (hash-ref ahash c)) difval))])))
    
    (let* ([alist (map string->number (string-split aline " "))]
           [h (make-hash)])
      (begin
        (hash-set! h 0 alist)
        (let ([h (to-hash alist 0 h)])
          (cond
            [(string=? side "r") (extrapolate-r h (foldl max 0 (hash-keys h)) 0)]
            [else (extrapolate-l h (foldl max 0 (hash-keys h)) 0)])
          )))))

(define testanswer (foldl + 0 (map extrapolate testlines)))
(define answer (foldl + 0 (map extrapolate lines)))


;; part two
;; seems extremely easy
(define testanswer2 (foldl + 0 (map (lambda (l) (extrapolate l "l")) testlines)))
(define answer2 (foldl + 0 (map (lambda (l) (extrapolate l "l")) lines)))
