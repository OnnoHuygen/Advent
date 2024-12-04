#!/usr/bin/racket
#lang racket

(define lines (file->lines "input"))
(define testlines (file->lines "test_input"))

(define instructions (map string (string->list (first lines))))
(define testinstructions (map string (string->list (first testlines))))

(define nodes (rest (rest lines)))
(define testnodes (rest (rest testlines)))

;; (defin
(define (make-net nodes)
  (local ((define (add-node net line)
            (let* ([in (string-split line " = ")]
                   [key (first in)]
                   [val (string-split (string-replace (string-replace (first (rest in)) "(" "") ")" "") ", ")])
              (hash-set! net key val)))
          (define (make-net net lines)
            (cond
              [(empty? lines) net]
              [else (begin
                      (add-node net (first lines))
                      (make-net net (rest lines)))])))
    (make-net (make-hash) nodes)))

(define network (make-net nodes))
(define testnetwork (make-net testnodes))

(define (step net pos instruction)
  (cond
    [(string=? instruction "L")
     (first (hash-ref net pos))]
    [(string=? instruction "R")
     (first (rest (hash-ref net pos)))]))

(define (step-until-done net instructions)
  (local ((define (step-until-done net instructions pos steps-taken)
            (cond
              [(string=? pos "ZZZ") steps-taken]
              [else (step-until-done net (append (rest instructions) (list (first instructions)))
                                      (step net pos (first instructions)) (+ steps-taken 1))])))
    (step-until-done net instructions "AAA" 0)))

;; ---------- part 2 ----------
(define testlines2 (file->lines "test_input2"))

(define testinstructions2 (map string (string->list (first testlines2))))

(define testnodes2 (rest (rest testlines2)))
(define testnetwork2 (make-net testnodes2))


(define (get-starting-nodes keys)
  (cond
    [(empty? keys) empty]
    [(string=? "A" (first (reverse (map string (string->list (first keys))))))
     (cons (first keys) (get-starting-nodes (rest keys)))] ;;ends in A
    [else (get-starting-nodes (rest keys))]))


(define (concurrent-steps net instructions)
  (local ((define (all-done? poslist)
            (cond
              [(empty? poslist) true]
              [(string=? "Z" (first (reverse (map string (string->list (first poslist))))))
               (all-done? (rest poslist))]
              [else false]))
          (define (cs net instructions poslist steps-taken)
            (cond
              [(all-done? poslist) steps-taken]
              [else (cs net (append (rest instructions) (list (first instructions)))
                                     (map (lambda (p) (step net p (first instructions))) poslist)
                                     (+ steps-taken 1))])))
    (cs net instructions (get-starting-nodes (hash-keys net)) 0)))
;; Since this takes a long time, they probably put at least two large prime-numbered cycles into the graph.

;; Save nodes visited WITH instructions at that node
;; It is only a cycle when we arrive at an already visited node with the same instructions we had when we previously visited that node.
;; history: hash string -> int
;; Key of history is appended string of pos and instructions at that moment
;; value is when this state was visited.
;; If this is very slow, I can easily make a sorted dictionary of this or make a search tree or something.
(define (get-cycle-properties start instructions net)
  (local ((define (get-cycle-properties pos instructions net steps-taken history z-positions)
            (let ([key (string-append pos (foldr string-append "" instructions))])
              (cond
                [(hash-has-key? history key) (list (hash-ref history key)
                                                   (- steps-taken (hash-ref history key))
                                                   z-positions)]
                [(string=? (first (reverse (map string (string->list pos)))) "Z")
                 (begin
                   (print steps-taken)
                   (hash-set! history key steps-taken)
                   (get-cycle-properties (step net pos (first instructions))
                                         (append (rest instructions) (list (first instructions)))
                                         net (+ steps-taken 1) history
                                         (append z-positions (list steps-taken))))]
                [else (begin
                        (hash-set! history key steps-taken)
                        (get-cycle-properties (step net pos (first instructions))
                                              (append (rest instructions) (list (first instructions)))
                                              net (+ steps-taken 1) history
                                              z-positions))]))))
    (get-cycle-properties start instructions net 0 (make-hash) empty)))

(define cycle-properties (map (lambda (p) (get-cycle-properties p instructions network)) (get-starting-nodes (hash-keys network))))
(define test-cycle-properties (map (lambda (p) (get-cycle-properties p testinstructions2 testnetwork2)) (get-starting-nodes (hash-keys testnetwork2))))
;; Every cycle contains only one single endpoint, which is coincidentally also the last node int he cycle. That makes things easier.
;; It takes 5 steps until every starting node is in its own cycle.
;; Then it takes 15517 steps until the second starting node is at an end node.
;; So take (5+15517) initially.

;; Now we perform sets of 15517 steps at once until every node is at and endnode.
;; So, we track a counter starting at (5+15517) incrementing by 15517
;; Check after each increment for the other cycles if the new counter minus burn in is divisible by their cycle length.
;; If this is true for all cycles, we are there.

(define cp (list (list 2 17621) (list 5 15517) (list 2 19199) (list 2 20777) (list 2 11309) (list 3 13939)))
;; ALL of these cycles have prime factor 263...
(define evo-after-sync (* 263 53 59 43 73 67 79))
;; WHY DOES THIS WORK?? I think this is wrong?
;; This would only work if the burn-in for everycycle length is the same.
;; This is NOT the case.
;; So after taking this amount of steps, I think e.g. the first starting point is still 2 off the end node,
;; and the second cycle is still 5 off the end node.
;; If all of them would be equally far removed from the end node, no problem
;; but again, this is not the case.
(define answer2 evo-after-sync)

;; Answer: a number of times such that
;; product of all unique prime factors of all cycle lengths
;; But i am not sure this is correct actually
;; There is burn-in for each cycle, as in the example. The burn in is different.
;; Thus, the cycles are not in sync. Not every cycle is exactly one cycle length removed

;; How about we go one back? Is that possible?
;; Go one back, 

(define (get-answer)
  (local ((define (at-end-node? cycle-properties counter)
            (cond
              [(= 0 (modulo (- counter (first cycle-properties)) (first (rest cycle-properties)))) true]
              [else false]))
          (define (get-answer c list-of-cycle-properties)
            (cond
              [(foldl (lambda (a b) (and a b)) true (map (lambda (n) (at-end-node? n c)) list-of-cycle-properties))
               c]
              [else (get-answer (+ c 15517) list-of-cycle-properties)])))
    (get-answer (+ 2 15517) cp)))

;; (define (get-answer)
;;   (local ((define (at-end-node? cycle-properties counter)
;;             (cond
;;               [(= 0 (modulo (- counter (first cycle-properties)) (first (rest cycle-properties)))) true]
;;               [else false]))
;;           (define (get-answer c list-of-cycle-properties)
;;             (cond
;;               [(foldl (lambda (a b) (and a b)) true (map (lambda (n) (at-end-node? n c)) list-of-cycle-properties))
;;                c]
;;               [else (get-answer (+ c 15517) list-of-cycle-properties)])))
;;     (get-answer (+ 2 15517) cp)))

;; hah this still takes equally long of course.
;; Let's take a different approach.

;; Take the longest burn in, that is (5 15517)
;; First progress chain by 5
;; All other cycles are in their cycle, and need progression of  (- cycle-length (- 5 cycle-burn-in)) to get to their end point.
;; Then we need to progress N*15517, where N is defined such that:
;; (= (modulo (* N 15517) cycle-length) (- cycle-length (- 5 cycle-burn-in)))

;; Progress by 5+15517
;; e.g. last cycle now needs further progression of (- (+ 3 13939) (+ 5 15517)) OR (+ (- (+ 3 13939) (+ 5 15517)) 13939) (this can be negative).
;; So we want some N for which  (- (+ 3 13939) (+ 5 15517)) 

(define (find-N burn-in cycle-length)
  (local ((define (find-N burn-in cycle-length N)
            (cond
              [(= (modulo (* N (modulo 15517 cycle-length)) cycle-length) (- cycle-length (- 5 burn-in))) N]
              [else (find-N burn-in cycle-length (+ N 1))])))
    (find-N burn-in cycle-length 1)))

;; (define cp-left (list (list 0 17621) (list 3 15517) (list 0 19199) (list 0 11309) (list 1 13939)))

(define li (map (lambda (e) (first (rest e))) cycle-properties)) ;; cycle lengths
(define bi (map first cycle-properties)) ;; Burn in times.

;; get prime factors
(define (factorize number)
  (local ((define (factorize number plist c)
            (cond
              [(= 1 number) plist]
              [(> c (sqrt number)) (cons number plist)]
              [(= (modulo number c) 0) (factorize (/ number c) (cons c plist) c)]
              [else (factorize number plist (+ c 1))])))
    (factorize number empty 2)))

;; Progress by (2 + 20777)
;; Then progress by 
(define progressions (map (lambda (l) (modulo 20777 l)) li))
(define remainders (map (lambda (c) (- (+ 20777 2) (+ (first c) (first (rest c))))) cycle-properties))
(define factorized-r (map factorize remainders))


(define start "GRA")
(define (take-n-steps point n [net network] [instruct instructions])
  (cond
    [(= 0 n) point]
    [else (take-n-steps (step net point (first instruct)) (- n 1) net (append (rest instruct) (list (first instruct))))]))
