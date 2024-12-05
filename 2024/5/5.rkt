#!/usr/bin/racket
#lang racket

(define lines (file->lines "input"))
(define testlines (file->lines "test_input"))

(define (split-input alist)
  (local ((define (split-input before after)
            (cond
              [(string=? "" (first after)) (list before (rest after))]
              [else (split-input (append before (list (first after))) (rest after))])))
    (split-input empty alist)))

(define testspl (split-input testlines))
(define testrules (first testspl))
(define testupdates (first (rest testspl)))
(define spl (split-input lines))
(define rules (first spl))
(define updates (first (rest spl)))
(define (rules->string rules)
  (cond
    [(empty? rules) ""]
    [else (string-append (first rules) "," (rules->string (rest rules)))]))

(define testrules (rules->string testrules))
(define rules (rules->string rules))

(define (is-valid? update-list rulestring);; slow way: scales O(n^2) with the size of each update. Scales linearly in nr of updates.
  (local ((define (first-entry-valid? entry update-rest combined-rules)
            (cond
              [(empty? update-rest) true]
              [(> (length (regexp-match* (regexp (string-append (first update-rest) "\\|" entry)) combined-rules)) 0) false]
              [else (first-entry-valid? entry (rest update-rest) combined-rules)]))
          (define (is-valid? update combined-rules)
            (cond
              [(empty? update) true]
              [(not (first-entry-valid? (first update) (rest update) combined-rules)) false]
              [else (is-valid? (rest update) combined-rules)])))
    (is-valid? update-list rulestring)))

(define (get-middle alist)
  (let ([ln (length alist)])
    (list-ref alist (inexact->exact (floor (/ ln 2))))))

(define (add-correct-updates updates rules)
  (cond
    [(empty? updates) 0]
    [else (let ([updatelist (string-split (first updates) ",")])
            (cond
              [(is-valid? updatelist rules) (+ (string->number (get-middle updatelist))
                                               (add-correct-updates (rest updates) rules))]
              [else (add-correct-updates (rest updates) rules)]))]))

(define testanswer (add-correct-updates testupdates testrules))
(define answer1 (add-correct-updates updates rules))

;; -------------- Part 2 ----------------------
;; conjecture:
;; Given rules and a set of numbers, we can just construct an ordering
;; At least one number should appear ONLY on the left side of all rules
;; Put all numbers for which that holds in the beginning of an update in arbitrary order
;; Remove all rules that contain that number, they are now obsolete
;; again at least one number should appear ONLY on the left side of all rules
;; iterate.
(define (correct-update update rules)
  (local ((define (get-relevant-rules update ruleslist) ;; filter only rules that contain relevant nrs
            (cond
              [(empty? update) empty]
              [else 
               (let* ([nrs-reg (foldr (lambda (s1 s2) (string-append s1 "|" s2)) (first update) (rest update))]
                      [reg (regexp (string-append "(" nrs-reg ")\\|(" nrs-reg ")"))])
                 (filter (lambda (el) (regexp-match reg el)) ruleslist))]))
          (define (is-first? entry ruleslist)
            (cond
              [(empty? ruleslist) true] ;; place in front
              [(regexp-match (regexp (string-append "\\|" entry)) (first ruleslist)) false]
              [else (is-first? entry (rest ruleslist))]))
          (define (correct-update beforelist update ruleslist)
            (cond
              [(and (empty? update) (empty? beforelist)) empty]
              [(is-first? (first update) ruleslist)
               (let* ([rest-update (append beforelist (rest update))]
                      [new-ruleslist (get-relevant-rules rest-update ruleslist)])
                 (cons (first update) (correct-update empty rest-update new-ruleslist)))]
              [else (correct-update (append beforelist (list (first update))) (rest update) ruleslist)])))
    (let ([up (string-split update ",")])
      (correct-update empty up (get-relevant-rules up (string-split rules ","))))))

(define (add-corrected-updates updatelist rules)
  (cond
    [(empty? updatelist) 0]
    [(not (is-valid? (string-split (first updatelist) ",") rules))
     (let ([corrected-update (correct-update (first updatelist) rules)])
       (+ (string->number (get-middle corrected-update))
          (add-corrected-updates (rest updatelist) rules)))]
    [else (add-corrected-updates (rest updatelist) rules)]))

(define testanswer2 (add-corrected-updates testupdates testrules))
(define answer2 (add-corrected-updates updates rules))
