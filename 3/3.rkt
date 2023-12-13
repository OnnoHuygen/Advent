#!/usr/bin/racket
#lang racket
(define lines (file->lines "input"))
(define testlines (file->lines "test_input"))


;; Make my own lookup
(define (lines->hash lines)
  (local ((define (aline->hash aline res c)
            (cond
              [(empty? aline) res]
              [else (begin
                      (hash-set! res c (first aline))
                      (aline->hash (rest aline) res (+ c 1)))]))
          (define (lines->hash lines res c)
            (cond
              [(empty? lines) res]
              [(begin
                 (hash-set! res c (aline->hash (map string (string->list (first lines))) (make-hash) 0))
                 (lines->hash (rest lines) res (+ c 1)))])))
    (lines->hash lines (make-hash) 0)))

(define mat (lines->hash lines))
(define testmat (lines->hash testlines))


(define-struct pos (row col))
(define-struct num (nr poslist)) ;; list of positions (i.e. posn of digits)

(define (get-pos pos [matrix mat])
  (local ((define (get-pos matrix i j)
            (cond
              [(or (> i (- (length (hash-keys matrix)) 1))
                   (< i 0)) "."]
              [(or (> j (- (length (hash-keys (hash-ref matrix i))) 1))
                   (< j 0))"."]
              [else (hash-ref (hash-ref matrix i) j)])))
    (get-pos matrix (pos-row pos) (pos-col pos))))

(define (is-symbol? pos [matrix mat])
  (let ([s (get-pos pos matrix)])
    (cond
      [(member s (cons "." (map number->string (range 10)))) false]
      [else true])))

(define (get-numbers aline linenr) ;; produce a list of num contained in line.
  (local ((define (get-numbers aline c linenr current-string current-poslist res)
            (cond
              [(empty? aline)
               (cond
                 [(empty? current-poslist) res]
                 [else (cons (make-num (string->number current-string) current-poslist) res)])]
              [else (let ([n (string->number (first aline))])
                      (cond
                        [(empty? aline) (cond
                                          [(empty? current-poslist) res]
                                          [else (cons (make-num (string->number current-string) current-poslist) res)])]
                        [n (get-numbers (rest aline)
                                        (+ c 1)
                                        linenr
                                        (string-append current-string (first aline))
                                        (append current-poslist (list (make-pos linenr c)))
                                        res)]
                        [(not (string=? "" current-string))
                         (get-numbers (rest aline)
                                      (+ c 1)
                                      linenr
                                      ""
                                      empty
                                      (cons (make-num (string->number current-string) current-poslist) res))]
                        [else (get-numbers (rest aline) (+ c 1) linenr current-string current-poslist res)]))])))
    (get-numbers (map string (string->list aline)) 0 linenr "" empty empty)))

(define (is-part? num [matrix mat])
  (local ((define (get-neighbors pos) ;; check if they are valid positions later on
            (let ([r (pos-row pos)]
                  [c (pos-col pos)])
              (list (make-pos r (- c 1))
                    (make-pos r (+ c 1))
                    (make-pos (- r 1) (- c 1))
                    (make-pos (- r 1) c)
                    (make-pos (- r 1) (+ c 1))
                    (make-pos (+ r 1) (- c 1))
                    (make-pos (+ r 1) c)
                    (make-pos (+ r 1) (+ c 1)))));; includes impossible points, but that is not 
          (define (is-part? num check-neighbors)
            (cond
              [(empty? check-neighbors) false]
              [else
               (let ([s (get-pos (first check-neighbors) matrix)])
                 (cond
                   [(is-symbol? (first check-neighbors) matrix) true]
                   [(member s (map number->string (range 10)))
                    (is-part? num (append (rest check-neighbors) (rest (get-neighbors (first check-neighbors)))))];; neighbor is a number, i.e. check
                   [else (is-part? num (rest check-neighbors))]))])))
    (is-part? num (get-neighbors (first (num-poslist num))))))

(define (sumline aline linenr [matrix mat])
  (foldl (lambda (n1 n2) (+ n1 n2)) 0 (map num-nr (filter (lambda (e) (is-part? e matrix)) (get-numbers aline linenr)))))

(define (get-answer lines [matrix mat])
  (local ((define (get-answer lines c)
            (cond
              [(empty? lines) 0]
              [else (+ (sumline (first lines) c matrix)
                       (get-answer (rest lines) (+ c 1)))])))
    (get-answer lines 0)))

;; Part two
(define (all-parts aline linenr [matrix mat])
  (filter (lambda (n) (is-part? n matrix)) (get-numbers aline linenr)))

(define (get-parts lines [matrix mat])
  (local ((define (get-parts lines c)
            (cond
              [(empty? lines) empty]
              [else (append (all-parts (first lines) c matrix)
                            (get-parts (rest lines) (+ c 1)))])))
    (get-parts lines 0)))

(define parts (get-parts lines))
(define testparts (get-parts testlines testmat))

;; position of all possible gears
(define (get-gears lines [matrix mat])
  (local ((define (get-gear-pos r c aline)
            (cond
              [(empty? aline) empty]
              [(string=? (first aline) "*")
               (cons (make-pos r c)
                     (get-gear-pos r (+ c 1) (rest aline)))]
              [else (get-gear-pos r (+ c 1) (rest aline))]))
          (define (get-gears lines linenr)
            (cond
              [(empty? lines) empty]
              [else (append (get-gear-pos linenr 0 (map string (string->list (first lines))))
                            (get-gears (rest lines) (+ linenr 1)))])))
    (get-gears lines 0)))

(define testgears (get-gears testlines testmat))
(define gears (get-gears lines))

(define (get-neighbors pos) ;; check if they are valid positions later on
            (let ([r (pos-row pos)]
                  [c (pos-col pos)])
              (list (make-pos r (- c 1))
                    (make-pos r (+ c 1))
                    (make-pos (- r 1) (- c 1))
                    (make-pos (- r 1) c)
                    (make-pos (- r 1) (+ c 1))
                    (make-pos (+ r 1) (- c 1))
                    (make-pos (+ r 1) c)
                    (make-pos (+ r 1) (+ c 1)))))

(define (pos=? p1 p2)
            (cond
              [(and (= (pos-row p1) (pos-row p2))
                    (= (pos-col p1) (pos-col p2))) true]
              [else false]))

(define (get-neighbor-parts gearpos partlist)
  (local ((define (get-neighbors pos) ;; check if they are valid positions later on
            (let ([r (pos-row pos)]
                  [c (pos-col pos)])
              (list (make-pos r (- c 1))
                    (make-pos r (+ c 1))
                    (make-pos (- r 1) (- c 1))
                    (make-pos (- r 1) c)
                    (make-pos (- r 1) (+ c 1))
                    (make-pos (+ r 1) (- c 1))
                    (make-pos (+ r 1) c)
                    (make-pos (+ r 1) (+ c 1)))))
          (define (pos=? p1 p2)
            (cond
              [(and (= (pos-row p1) (pos-row p2))
                    (= (pos-col p1) (pos-col p2))) true]
              [else false]))
          (define (is-neighbor? gearpos part)
            (cond
              [(member gearpos (foldl append empty (map get-neighbors (num-poslist part))) pos=?) true]
              [else false]))
          (define (get-neighbor-parts gearpos partlist)
            (filter (lambda (p) (is-neighbor? gearpos p)) partlist)))
    (get-neighbor-parts gearpos partlist)))

(define (get-answer gearlist partlist)
  (local ((define (gear-ratio gear partlist)
            (let ([prts (get-neighbor-parts gear partlist)])
              (cond
                [(= 2 (length prts)) (foldl * 1 (map num-nr prts))]
                [else 0])))
          (define (get-answer gearlist partlist)
            (cond
              [(empty? gearlist) 0]
              [else (+ (gear-ratio (first gearlist) partlist)
                       (get-answer (rest gearlist) partlist))])))
    (get-answer gearlist partlist)))
              
