#!/usr/bin/racket
#lang racket

(define testlines (file->lines "test_input"))
(define lines (file->lines "input"))

(define (lines->lists ln)
  (map (lambda (s) (map string->number (string-split s " "))) ln))

(define (list-to-diffs alist)
  (cond
    [(or (empty? alist)
         (empty? (rest alist))) empty]
    [else (cons (- (first alist) (first (rest alist)))
                (list-to-diffs (rest alist)))]))

(define (safe? difflist)
  (cond
    [(or
      (and (empty? (filter (lambda (el) (> el 0)) difflist))
           (empty? (filter (lambda (el) (> (abs el) 3)) difflist))
           (empty? (filter (lambda (el) (= 0 el)) difflist)))
      (and (empty? (filter (lambda (el) (< el 0)) difflist))
           (empty? (filter (lambda (el) (> (abs el) 3)) difflist))
           (empty? (filter (lambda (el) (= 0 el)) difflist)))) true]
    [else false]))
(define testanswer1 (length (filter (lambda (el) el)
                                    (map safe? (map list-to-diffs (lines->lists testlines))))))
(define answer1 (length (filter (lambda (el) el)
                                (map safe? (map list-to-diffs (lines->lists lines))))))

;; (define (almost-safe? alist)
;;   (local ((define (check-pairwise alist sign errors-left)
;;             (cond
;;               [(or (empty? alist)
;;                    (empty? (rest alist))) true]
;;               [(not (= (sgn (- (first alist) (first (rest alist))))))]

;; just do it the ugly way brute forcing every number, there are only a couple of numbers per line

(define (almost-safe? alist)
  (local ((define (almost-safe? front-part back-part)
            (cond
              [(empty? back-part) false]
              [(safe? (list-to-diffs (append front-part (rest back-part)))) true]
              [else (almost-safe? (append front-part (list (first back-part))) (rest back-part))])))
    (almost-safe? empty alist)))

(define testanswer2 (length (filter (lambda (el) el)
                                    (map almost-safe? (lines->lists testlines)))))
(define answer2 (length (filter (lambda (el) el)
                                (map almost-safe? (lines->lists lines)))))
