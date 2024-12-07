#!/usr/bin/racket
#lang racket

(define lines (file->lines "input"))
(define testlines (file->lines "test_input"))

(define (parse-line aline)
  (let* ([spl (string-split aline ":")]
         [components (string-split (first (rest spl)) " ")])
    (list (string->number (first spl)) (map string->number components))))


;; Try 1, 
;; try 1 failed... I have no clue what is wrong...
;; Perhaps this is based on a wrong conjecture...
;; If the value is already higher than the testval, new combinations can only be even higher
;; So new operator combinations are not created.
;; wait is it wrong? this should be correct

(define (pluses n)
  (cond
    [(= 0 n) empty]
    [else (cons + (pluses (- n 1)))]))

(define (check-possibilities testval components)
  (local ((define (combine components operators)
            (cond
              [(empty? (rest (rest components)))
               ((first operators) (first components) (first (rest components)))]
              [else
               (combine (append (list ((first operators) (first components) (first (rest components)))) (rest (rest components)))
                        (rest operators))]))
          (define (create-new-operators operators pre-list) ;; Create new possibilities by replacing all +-s with *s
            (cond
              [(empty? operators) empty]
              [(equal? (first operators) +)
               (cons (append pre-list (list *) (rest operators))
                     (create-new-operators (rest operators) (append pre-list (list (first operators)))))]
              ;[else (create-new-operators (rest operators) (append pre-list (list (first operators))))]))
              [else empty]))
          (define (check-possibilities testval components list-of-operators)
            (cond
              [(empty? list-of-operators) false]
              [else
               (let ([tst (combine components (first list-of-operators))])
                 (cond
                   [(= tst testval) true]
                   [(> tst testval) (check-possibilities testval components (rest list-of-operators))]
                   [else (check-possibilities testval components (append (rest list-of-operators)
                                                                         (create-new-operators (first list-of-operators) empty)))]))])))
    (check-possibilities testval components (list (pluses (- (length components) 1))))))

(define (check-line aline)
  (let* ([spl (parse-line aline)]
         [tv (first spl)]
         [comp (first (rest spl))])
    (check-possibilities tv comp)))

(define (add-correct-lines-old lines-list)
  (foldr + 0 (map (lambda (aline) (string->number (first (string-split aline ":")))) (filter check-line lines-list))))

(define (add-lines lns)
  (begin
    (print (length lns))
    (print " ")
    (cond
      [(empty? lns) 0]
      [(check-line (first lns))
       (+ (string->number (first (string-split (first lns) ":")))
          (add-lines (rest lns)))]
      [else (add-lines (rest lns))])))

;; (define testanswer (add-correct-lines testlines)) wrong...
;; (define answer1 (add-correct-lines lines)) wrong...


;; Can this be done in a smarter way?
;; One thing we can do is test if the testvalue is divisible by the last component.
;; If it is not, then either the last component was added, or the testval cannot be created from the components.
;; If it is, we still need to check both possibilities though...

;; Try 2

(define (peel testval reversed-list-of-components)
  (cond
    [(or (empty? reversed-list-of-components)
         (< testval 0)) false]
    [(= testval (first reversed-list-of-components)) true]
    [(= 0 (modulo testval (first reversed-list-of-components)))
     (or
      (peel (/ testval (first reversed-list-of-components)) (rest reversed-list-of-components))
      (peel (- testval (first reversed-list-of-components)) (rest reversed-list-of-components)))]
    [else (peel (- testval (first reversed-list-of-components)) (rest reversed-list-of-components))]))

(define (check-line2 aline)
  (let* ([spl (parse-line aline)]
         [tv (first spl)]
         [comp (first (rest spl))])
    (peel tv (reverse comp))))


(define (add-correct-lines lines-list)
  (foldr + 0 (map (lambda (aline) (string->number (first (string-split aline ":")))) (filter check-line2 lines-list))))



(define testanswer (add-correct-lines testlines))
(define answer1 (add-correct-lines lines))

;; Check where my first try went wrong:
(define diff (filter (lambda (l) (not (equal? (check-line l) (check-line2 l)))) lines))


;; ------ Part 2 ----
;; Since we are working with inverse operators, we need to de-concatenate as well.

(define (de-concatenate testval comp)
  (let ([tvstring (number->string testval)]
        [rx (regexp (string-append (number->string comp) "$"))])
    (cond
      [(not (regexp-match rx tvstring))
       false]
      [else (string->number (regexp-replace rx tvstring ""))])))

(define (peel2 testval reversed-list-of-components)
  (cond
    [(or (empty? reversed-list-of-components)
         (< testval 0)) false]
    [(= testval (first reversed-list-of-components)) true]
    [else (let ([decat (de-concatenate testval (first reversed-list-of-components))])
            (cond
              [(and decat (= (modulo testval (first reversed-list-of-components)) 0))
               (or (peel2 (/ testval (first reversed-list-of-components)) (rest reversed-list-of-components))
                   (peel2 (- testval (first reversed-list-of-components)) (rest reversed-list-of-components))
                   (peel2 decat (rest reversed-list-of-components)))]
              [(= (modulo testval (first reversed-list-of-components)) 0)
               (or
                (peel2 (/ testval (first reversed-list-of-components)) (rest reversed-list-of-components))
                (peel2 (- testval (first reversed-list-of-components)) (rest reversed-list-of-components)))]
              [decat
               (or (peel2 decat (rest reversed-list-of-components))
                   (peel2 (- testval (first reversed-list-of-components)) (rest reversed-list-of-components)))]
              [else (peel2 (- testval (first reversed-list-of-components)) (rest reversed-list-of-components))]))]))

(define (check-line3 aline)
  (let* ([spl (parse-line aline)]
         [tv (first spl)]
         [comp (first (rest spl))])
    (peel2 tv (reverse comp))))

(define (add-correct-lines2 lines-list)
  (foldr + 0 (map (lambda (aline) (string->number (first (string-split aline ":")))) (filter check-line3 lines-list))))

(define testanswser2 (add-correct-lines2 testlines))
(define answer2 (add-correct-lines2 lines)) ;; this is very fast, 76ms.
