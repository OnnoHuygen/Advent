#!/usr/bin/racket
#lang racket

(define lines (file->lines "input"))
(define testlines (file->lines "test_input"))

;; make my own matrix type
(define (lines->hash lines)
  (local ((define (aline->hash aline res c)
            (cond
              [(empty? aline) res]
              [else (aline->hash (rest aline) (hash-set res c (first aline)) (+ c 1))]))
          (define (lines->hash lines res c)
            (cond
              [(empty? lines) res]
              [(lines->hash (rest lines) (hash-set res c (aline->hash (map string (string->list (first lines))) (make-immutable-hash) 0)) (+ c 1))])))
    (lines->hash lines (make-immutable-hash) 0)))

(define mat (lines->hash lines))
(define testmat (lines->hash testlines))

;; Convert matrix to list of strings
(define (matrix->strings amat)
  (local ((define (line->string aline c)
            (let ([value (hash-ref aline c false)])
              (cond
                [(not value) ""]
                [else (string-append value (line->string aline (+ c 1)))])))
          (define (matrix->strings amat c)
            (let ([row (hash-ref amat c false)])
              (cond
                [(not row) empty]
                [else (append (list (line->string row 0))
                              (matrix->strings amat (+ c 1)))]))))
    (matrix->strings amat 0)))

;; Transpose of matrix
(define (transpose amat)
  (local ((define (make-row amat column-number c)
            (let ([old-row (hash-ref amat c false)])
              (cond
                [(not old-row) (make-immutable-hash)]
                [else (hash-set (make-row amat column-number (+ c 1))
                                c (hash-ref old-row column-number))])));; not tail recursive, can be optimized
          (define (make-transpose amat column-number)
            (let ([v1 (hash-ref (hash-ref amat 0) column-number false)])
              (cond
                [(not v1) (make-immutable-hash)]
                [else (hash-set (make-transpose amat (+ column-number 1));; also not tail recursive, can be optimized also
                                column-number (make-row amat column-number 0))]))))
    (make-transpose amat 0)))

;;Rotation of matrix
(define (rotate amat)
  (local ((define (column-to-row amat colnr c)
            (let ([row (hash-ref amat c false)])
              (cond
                [(not row) (make-immutable-hash)]
                [else (hash-set (column-to-row amat colnr (+ c 1));; Am I ever going to make this tail-recursive?
                                c (hash-ref row colnr))])))
          (define (rotate amat c l)
            (cond
              [(< c 0) (make-immutable-hash)]
              [else (hash-set (rotate amat (- c 1) l) (- l c) (column-to-row amat c 0))])))
    (rotate amat (- (length (hash-keys (hash-ref amat 0))) 1) (- (length (hash-keys (hash-ref amat 0))) 1))))
              

;; Get diagonals in similar lookup matrix (non-rectangular matrix, diamond shaped I guess)
(define (diagonals amat)
  (local ((define (get-diagonal amat xpos ypos c)
            (let ([val (hash-ref (hash-ref amat ypos (make-immutable-hash)) xpos false)])
              (cond
                [(not val) (make-immutable-hash)]
                [else (hash-set (get-diagonal amat (+ xpos 1) (- ypos 1) (+ c 1)) ;; Apparently, I do not like to make things tail-recursive
                                c val)])))
          (define (diagonals amat xpos ypos c increment-mode)
            (let ([row (hash-ref amat ypos false)])
              (cond
                [(not row) (diagonals amat 1 (- ypos 1) c 1)]
                [(and (= increment-mode 1)
                      (not (hash-ref row xpos false))) (make-immutable-hash)]
                [(= increment-mode 0)
                 (hash-set (diagonals amat xpos (+ ypos 1) (+ c 1) 0)
                           c (get-diagonal amat xpos ypos 0))]
                [(= increment-mode 1)
                 (hash-set (diagonals amat (+ xpos 1) ypos (+ c 1) 1)
                           c (get-diagonal amat xpos ypos 0))]))))
    (diagonals amat 0 0 0 0)))


(define (string-count-xmas astring)
  (length (regexp-match* #rx"(?=(XMAS|SAMX))" astring)))
            
(define (mat-count-xmas amat)
  (foldr + 0 (map string-count-xmas (matrix->strings amat))))

(define (xmas-count amat)
  (+ (mat-count-xmas amat)
     (mat-count-xmas (transpose amat))
     (mat-count-xmas (diagonals amat))
     (mat-count-xmas (diagonals (rotate amat)))))

(define testanswer (xmas-count testmat))
(define answer1 (xmas-count mat))

;; Part 2
;; Iterate over all entries (except outermost)
;; Check if values on cross points are twice M and S and M neighboring M

(define (x-mas-count amat)
  (local ((define (is-x-mas? amat xpos ypos)
            (let* ([ul (hash-ref (hash-ref amat (- ypos 1)) (- xpos 1))]
                   [ur (hash-ref (hash-ref amat (- ypos 1)) (+ xpos 1))]
                   [lr (hash-ref (hash-ref amat (+ ypos 1)) (+ xpos 1))]
                   [ll (hash-ref (hash-ref amat (+ ypos 1)) (- xpos 1))]
                   [cornerstring (string-append ul ur lr ll)])
              (cond
                [(not (string=? (hash-ref (hash-ref amat ypos) xpos) "A")) false]
                [(and (= (length (regexp-match* #rx"M" cornerstring)) 2)
                      (= (length (regexp-match* #rx"S" cornerstring)) 2)
                      (= (length (regexp-match* #rx"SMSM|MSMS" cornerstring)) 0)) true]
              [else false])))
          (define (x-mas-count amat xpos ypos xmax ymax)
            (cond
              [(>= ypos ymax) 0]
              [(>= xpos xmax) (x-mas-count amat 1 (+ ypos 1) xmax ymax)]
              [(is-x-mas? amat xpos ypos)
               (+ 1 (x-mas-count amat (+ xpos 1) ypos xmax ymax))]
              [else (x-mas-count amat (+ xpos 1) ypos xmax ymax)])))
    (x-mas-count amat 1 1 (- (length (hash-keys (hash-ref amat 0 (make-immutable-hash)))) 1) (- (length (hash-keys amat)) 1))))
