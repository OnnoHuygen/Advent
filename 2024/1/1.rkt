#!/usr/bin/racket
#lang racket

(define testlines (file->lines "test_input"))
(define lines (file->lines "input"))

(define (lines->lists ln)
  (local ((define (string->values astring)
            (list (string->number (first (string-split astring " ")))
                  (string->number (last (string-split astring " ")))))
          (define (lines->lists ln l1 l2)
            (cond
              [(empty? ln) (list l1 l2)]
              [else
               (let ([nrs (string->values (first ln))])
                 (lines->lists (rest ln)
                               (append l1 (list (first nrs)))
                               (append l2 (list [first (rest nrs)]))))])))
    (lines->lists ln empty empty)))

(define (quicksort alist)
  (cond
    [(empty? alist) alist]
    [else (let ([piv (first alist)])
            (append
             (quicksort (filter (lambda (el) (< el piv)) (rest alist)))
             (list piv)
             (quicksort (filter (lambda (el) (>= el piv)) (rest alist)))))]))

(define (list-diff l1 l2 [op (lambda (e1 e2) (abs (- e1 e2)))] [acc +])
  (cond
    [(or (empty? l1) (empty? l2)) 0]
    [else (acc (op (first l1) (first l2))
               (list-diff (rest l1) (rest l2)))]))

(define testanswer (let* ([lists (lines->lists testlines)]
                       [l1 (quicksort (first lists))]
                       [l2 (quicksort (first (rest lists)))])
                  (list-diff l1 l2)))

(define answer1 (let* ([lists (lines->lists lines)]
                       [l1 (quicksort (first lists))]
                       [l2 (quicksort (first (rest lists)))])
                  (list-diff l1 l2)))

;; Still assume sorted lists, make this procedure a bit quicker
(define (similarity-score l1 l2)
  (local ((define (get-count el sorted-list)
            (cond
              [(or (empty? sorted-list)
                   (> el (first sorted-list))) 0]
              [(= el (first sorted-list))
               (+ 1 (get-count el (rest sorted-list)))]
              [(< el (first sorted-list))
               (get-count el (rest sorted-list))]))
          (define (similarity-score l1 l2)
            (cond
              [(or (empty? l1) (empty? l2)) 0]
              [(> (first l1) (first l2))
               (similarity-score l1 (rest l2))]
              [(= (first l1) (first l2))
               (+ (* (first l1) (get-count (first l1) l2))
                  (similarity-score (rest l1) l2))]
              [(< (first l1) (first l2))
               (similarity-score (rest l1) l2)])))
    (similarity-score l1 l2)))

(define testanswer2 (let* ([lists (lines->lists testlines)]
                           [l1 (quicksort (first lists))]
                           [l2 (quicksort (first (rest lists)))])
                      (similarity-score l1 l2)))


(define answer2 (let* ([lists (lines->lists lines)]
                       [l1 (quicksort (first lists))]
                       [l2 (quicksort (first (rest lists)))])
                  (similarity-score l1 l2)))

