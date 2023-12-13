#!/usr/bin/racket
#lang racket
(define lines (file->lines "input"))
(define testlines (file->lines "test_input"))

;; can re-use some functions from before
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

;; (define testuniverse (lines->hash testlines))
;; (define universe (lines->hash lines))

(define (get-tile amat x y)
  (hash-ref (hash-ref amat y) x))

(define (qsort alist)
  (cond
    [(empty? alist) empty]
    [else (let ([piv (first alist)])
            (append (qsort (filter (lambda (el) (< el piv)) (rest alist)))
                    (list (first alist))
                    (qsort (filter (lambda (el) (>= el piv)) (rest alist)))))]))

;; Expand the universe
(define (expand universe)
  (local ((define (col-empty? universe x y ymax)
            (cond
              [(> y ymax) true]
              [(string=? (get-tile universe x y) ".")
               (col-empty? universe x (+ y 1) ymax)]
              [else false]))
          (define (empty-cols universe c)
            (let ([ymax (- (length (hash-keys universe)) 1)])
              (cond
                [(> c ymax) empty]
                [(col-empty? universe c 0 ymax)
                 (cons c (empty-cols universe (+ c 1)))]
                [else (empty-cols universe (+ c 1))])))
          (define (empty-rows universe r) ;; init c as highest key value
            (cond
              [(< r 0) empty]
              [(= 0 (length (filter (lambda (el) (string=? "#" el)) (hash-values (hash-ref universe r)))))
               (cons r (empty-rows universe (- r 1)))]
              [else (empty-rows universe (- r 1))]))
          (define (insert-col universe index x)
            (cond
              [(> index (- (length (hash-keys universe)) 1)) universe]
              [else (begin (hash-set! (hash-ref universe index) x ".")
                           (insert-col universe (+ index 1) x))]))
          (define (insert-row universe yval)
            (begin (hash-set! universe yval
                              (make-hash (map (lambda (el) (cons el "."))
                                              (range (length (hash-keys (hash-ref universe 0)))))))
                   universe))

          (define (rename-keys universe new-keys old-keys) ;; sorted from high to low both. This is important.
            (cond
              [(or (empty? new-keys)
                   (= (first new-keys) (first old-keys))) universe]
              [else (begin
                      (hash-set! universe (first new-keys) (hash-ref universe (first old-keys)))
                      (hash-remove! universe (first old-keys))
                      (rename-keys universe (rest new-keys) (rest old-keys)))]))
          (define (rename-rows universe c)
            (cond
              [(< c 0) universe]
              [else
               (begin (hash-set! universe c
                                 (rename-keys (hash-ref universe c)
                                              (reverse (range (length (hash-keys (hash-ref universe c)))))
                                              (reverse (qsort (hash-keys (hash-ref universe c))))))
                      (rename-rows universe (- c 1)))]))
          (define (expand universe collist rowlist)
            (cond
              [(and (empty? collist)
                    (empty? rowlist))
               (rename-rows (rename-keys universe (reverse (range (length (hash-keys universe))))
                                         (reverse (qsort (hash-keys universe))))
                            (- (length (hash-keys universe)) 1))]
              [(empty? collist)
               (expand (insert-row universe (+ (first rowlist) 0.5)) collist (rest rowlist))]
              [else (expand (insert-col universe 0 (+ (first collist) 0.5)) (rest collist) rowlist)])))
    (expand universe (empty-cols universe 0)
            (empty-rows universe (- (length (hash-keys universe)) 1)))))
              
;; (define expanded-test (expand testuniverse))
;; (define expanded (expand universe))

(define (get-galaxies universe)
  (local ((define (get-galaxies-from-row arow c yval)
            (cond
              [(< c 0) empty]
              [(string=? (hash-ref arow c) "#")
               (cons (list c yval) (get-galaxies-from-row arow (- c 1) yval))]
              [else (get-galaxies-from-row arow (- c 1) yval)]))
          (define (get-galaxies universe y)
            (cond
              [(< y 0) empty]
              [else (let ([row (hash-ref universe y)])
                      (append (get-galaxies-from-row row (- (length (hash-keys row)) 1) y)
                              (get-galaxies universe (- y 1))))])))
    (get-galaxies universe (- (length (hash-keys universe)) 1))))

(define (dist x1 y1 x2 y2)
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(define (get-answer universe)
  (local ((define (get-answer locations1 locations2)
            (cond
              [(empty? (rest locations1)) 0]
              [(empty? locations2)
               (get-answer (rest locations1) (rest (rest locations1)))]
              [else (+ (dist (first (first locations1)) (first (rest (first locations1)))
                             (first (first locations2)) (first (rest (first locations2))))
                       (get-answer locations1 (rest locations2)))])))
    (let ([locations (get-galaxies universe)])
      (get-answer locations (rest locations)))))

;; (define answer1 (get-answer expanded))

;; Part two
;; Don't think it is feasible to just actually expand the universe.
;; Rather, we can easily compute the distance between two points in the original map, then just add N for every empty row and column in between these two values where N is the expansion rate.
;; this could have been so easy...

(define testuniverse (lines->hash testlines))
(define universe (lines->hash lines))

;; Re-use some code from before
(define (col-empty? universe x y ymax)
  (cond
    [(> y ymax) true]
    [(string=? (get-tile universe x y) ".")
     (col-empty? universe x (+ y 1) ymax)]
    [else false]))

(define (empty-cols universe)
  (local ((define (empty-cols universe c)
            (let ([ymax (- (length (hash-keys universe)) 1)])
              (cond
                [(> c ymax) empty]
                [(col-empty? universe c 0 ymax)
                 (cons c (empty-cols universe (+ c 1)))]
                [else (empty-cols universe (+ c 1))]))))
    (empty-cols universe 0)))

(define (empty-rows universe)
  (local ((define (empty-rows universe r) ;; init r as highest key value
            (cond
              [(< r 0) empty]
              [(= 0 (length (filter (lambda (el) (string=? "#" el)) (hash-values (hash-ref universe r)))))
               (cons r (empty-rows universe (- r 1)))]
              [else (empty-rows universe (- r 1))])))
    (empty-rows universe (- (length (hash-keys universe)) 1))))


(define (new-dist x1 y1 x2 y2 empty-rows empty-cols expansion-rate)
  (+ (abs (- x1 x2))
     (abs (- y1 y2))
     (* (- expansion-rate 1)
        (length (filter (lambda (c) (and (< c (max x2 x1))
                                         (> c (min x2 x1))))
                        empty-cols)))
     (* (- expansion-rate 1)
        (length (filter (lambda (r) (and (< r (max y2 y1))
                                         (> r (min y2 y1))))
                        empty-rows)))))

(define (get-answer2 universe expansion-rate)
  (local ((define (get-answer locations1 locations2 erows ecols)
            (cond
              [(empty? (rest locations1)) 0]
              [(empty? locations2)
               (get-answer (rest locations1) (rest (rest locations1)) erows ecols)]
              [else (let ([d (new-dist (first (first locations1)) (first (rest (first locations1)))
                                       (first (first locations2)) (first (rest (first locations2)))
                                 erows
                                 ecols
                                 expansion-rate)])
                      (+ d (get-answer locations1 (rest locations2) erows ecols)))])))
    (let ([locations (get-galaxies universe)]
          [erows (empty-rows universe)]
          [ecols (empty-cols universe)])
      (get-answer locations (rest locations) erows ecols))))

(define answer1 (get-answer2 universe 2))
(define answer2 (get-answer2 universe 1000000))
;; (define ecols (empty-cols testuniverse))
;; (define erows (empty-rows testuniverse))

;; (define expanded (expand (lines->hash testlines)))

;; (define locations (get-galaxies universe))
;; (define locations-expanded (get-galaxies expanded))
