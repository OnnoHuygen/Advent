#!/usr/bin/racket
#lang racket
(require racket/port)

;; Get roots of quadratic equation
(define (roots time_total target_distance)
  (list (/ (- time_total (sqrt (- (expt time_total 2) (* 4 target_distance)))) 2)
        (/ (+ time_total (sqrt (- (expt time_total 2) (* 4 target_distance)))) 2)))

;; (define (distance_traveled time_total time_pressed)
;;   (* (- time_total time_pressed)

;; I want the integers between the floored first number and the ceiling-ed second number
(define (between-ints n1 n2)
  (local ((define (between-ints n2 acc)
            (cond
              [(>= acc n2) 0]
              [else (+ 1 (between-ints n2 (+ acc 1)))])))
    (between-ints n2 (+ (floor n1) 1))))
              

(define (number_options total_time target_distance)
  (let ([rt (roots total_time target_distance)])
    (between-ints (first rt) (first (rest rt)))))

(define lines (file->lines "input"))
(define times (map string->number (rest (filter (lambda (s) (not (string=? s ""))) (map (lambda (s) (string-replace s " " "")) (string-split (first lines) " "))))))
(define distances (map string->number (rest (filter (lambda (s) (not (string=? s ""))) (map (lambda (s) (string-replace s " " "")) (string-split (first (rest lines)) " "))))))

(define (options timelist distlist)
  (cond
    [(empty? timelist) empty]
    [else (cons (number_options (first timelist) (first distlist)) (options (rest timelist) (rest distlist)))]))

(define answer1 (foldl * 1 (options times distances)))

;; part two
(define tm (string->number (foldr string-append "" (rest (filter (lambda (s) (not (string=? s ""))) (map (lambda (s) (string-replace s " " "")) (string-split (first lines) " ")))))))
(define distance (string->number (foldr string-append "" (rest (filter (lambda (s) (not (string=? s ""))) (map (lambda (s) (string-replace s " " "")) (string-split (first (rest lines)) " ")))))))

(define answer2 (number_options tm distance))
