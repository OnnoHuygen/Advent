#!/usr/bin/racket
#lang racket

(define testinstruction (first (file->lines "test_input")))
(define instructions (file->lines "input"))

(define (lines->lists ln)
  (map (lambda (s) (map string->number (string-split s " "))) ln))

(define (get-mults astring)
  (let* ([matches (regexp-match* #px"mul\\((\\d|\\d\\d|\\d\\d\\d),(\\d|\\d\\d|\\d\\d\\d)\\)" astring)]
         [numberstrings (map (lambda (str) (regexp-match* #px"(\\d,|\\d\\d,|\\d\\d\\d,)|(\\d\\)|\\d\\d\\)|\\d\\d\\d\\))" str)) matches)]
         [nrs (map (lambda (el) (map (lambda (e) (string->number (regexp-replace #rx"(,|\\))" e ""))) el)) numberstrings)])
    (foldl + 0 (map (lambda (el) (* (first el) (first (rest el)))) nrs))))

(define testanswer1 (get-mults testinstruction))
(define answer1 (foldl + 0 (map get-mults instructions)))


(define testinstruction2 (first (file->lines "test_input2")))

(define (get-instructions astring)
  (regexp-match* #px"mul\\((\\d|\\d\\d|\\d\\d\\d),(\\d|\\d\\d|\\d\\d\\d)\\)|do\\(\\)|don't\\(\\)" astring))

(define (remove-disabled alist)
  (local ((define (remove-disabled alist mode)
            (cond
              [(empty? alist) empty]
              [(string=? (first alist) "don't()")
               (remove-disabled (rest alist) 0)]
              [(string=? (first alist) "do()")
               (remove-disabled (rest alist) 1)]
              [(= mode 1)
               (cons (first alist) (remove-disabled (rest alist) mode))]
              [(= mode 0)
               (remove-disabled (rest alist) mode)])))
    (remove-disabled alist 1)))

(define (get-mults2 list-of-instructions)
  (let* ([all-instructions (map (lambda (el) (get-instructions el)) list-of-instructions)]
         [list-of-mults (remove-disabled (foldr append empty all-instructions))]
         [numberstrings (map (lambda (str) (regexp-match* #px"(\\d,|\\d\\d,|\\d\\d\\d,)|(\\d\\)|\\d\\d\\)|\\d\\d\\d\\))" str)) list-of-mults)]
         [nrs (map (lambda (el) (map (lambda (e) (string->number (regexp-replace #rx"(,|\\))" e ""))) el)) numberstrings)])
    (foldr + 0 (map (lambda (el) (* (first el) (first (rest el)))) nrs))))

(define answer2 (get-mults2 instructions))
