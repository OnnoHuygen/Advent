#!/usr/bin/racket
#lang racket
(require racket/port)

;; A list of digits as strings
(define digits
  (local ((define (dstrings c)
            (cond
              [(> c 9) empty]
              [else (cons (number->string c) (dstrings (+ c 1)))])))
    (dstrings 0)))

;; A list of digits and their spelled out variants as strings
(define digits-with-spelling
  (local ((define spelled-digits
            (list "zero" "one" "two" "three" "four" "five"
                  "six" "seven" "eight" "nine")))
    (append digits spelled-digits)))

;; to-digit: litst-of-characters -> string
;; Return string of digit from list of characters
(define (to-digit charlist)
  (let* ([astring (list->string charlist)]
         [sn (string->number astring)]
         [rev (list->string (reverse charlist))])
    (cond
      [sn astring]
      [(or (string=? astring "zero") (string=? rev "zero")) "0"]
      [(or (string=? astring "one") (string=? rev "one")) "1"]
      [(or (string=? astring "two") (string=? rev "two")) "2"]
      [(or (string=? astring "three") (string=? rev "three")) "3"]
      [(or (string=? astring "four") (string=? rev "four")) "4"]
      [(or (string=? astring "five") (string=? rev "five")) "5"]
      [(or (string=? astring "six") (string=? rev "six")) "6"]
      [(or (string=? astring "seven") (string=? rev "seven")) "7"]
      [(or (string=? astring "eight") (string=? rev "eight")) "8"]
      [(or (string=? astring "nine") (string=? rev "nine")) "9"]
      [else false])))

;; starts-with?: list-of-characters list-of-characters -> bool
;; Checks if second list of characters starts with the first list of characters
(define (starts-with? subl alist)
  (cond
    [(empty? subl) true]
    [(empty? alist) false]
    [(char=? (first subl) (first alist)) (starts-with? (rest subl) (rest alist))]
    [else false]))

;; get-digit: list-of-characters list-of-lists-of-characters -> digitstring
;; To get a string of the digit contained in comparelist that appears first in the list-of-characters.
(define (get-digit al comparelist)
  (local ((define (check-all-digits alist digitlist)
            (cond
              [(empty? digitlist) (list false 0)]
              [(starts-with? (first digitlist) alist) (list true (to-digit (first digitlist)))]
              [else (check-all-digits alist (rest digitlist))]))
          (define (first-digit alist comparelist substr)
            (let ([result (check-all-digits (reverse substr) comparelist)])
              (cond
                [(first result) (first (rest result))]
                [(empty? alist) false]
                [else (first-digit (rest alist) comparelist (append substr (list (first alist))))]))))
    (first-digit (rest al) comparelist (list (first al)))))

;; get-number: string list-of-strings -> int
;; find the first substring in string that is an element of list-of-strings and the last substring
;; Turn them into numbers and make a two-digit number out of the results
(define (get-number astr [includelist digits-with-spelling])
  (let* ([dlist (map string->list includelist)]
         [rev-dlist (map reverse dlist)]
         [d1 (get-digit (string->list astr) rev-dlist)]
         [d2 (get-digit (reverse (string->list astr)) dlist)])
    (string->number (string-append d1 d2))))

;; test answer 2
(define test-answer2 (foldl + 0 (map get-number (file->lines "1_test_input"))))
;; answer 2 and 1 can be obtained by providing different lists of substrings that we are searching for.
(define answer2 (foldl + 0 (map get-number (file->lines "1_input"))))
(define answer1 (foldl + 0 (map (lambda (lst) (get-number lst digits)) (file->lines "1_input"))))
