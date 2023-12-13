#!/usr/bin/racket
#lang racket

(define lines (file->lines "input"))
(define testlines (file->lines "test_input"))
(define seeds (map string->number (rest (string-split (first lines) " "))))
(define testseeds (map string->number (rest (string-split (first testlines) " "))))

(define maps (rest (rest lines)))
(define testmaps (rest (rest testlines)))

(define (get-destination inputnr destination-numbers input-numbers ranges)
  (local ((define (in-range? in ins r)
            (cond
              [(and (>= in ins) (< in (+ ins r))) true]
              [else false]))
          (define (get-destination inputnr destination-numbers input-numbers ranges)
            (cond
              [(empty? destination-numbers) inputnr]
              [(in-range? inputnr (first input-numbers) (first ranges))
               (+ (first destination-numbers) (- inputnr (first input-numbers)))]
              [else (get-destination inputnr
                                     (rest destination-numbers)
                                     (rest input-numbers)
                                     (rest ranges))])))
    (get-destination inputnr destination-numbers input-numbers ranges)))

;; Map
(define-struct amap (dest in r))

(define (applymap mp inputnr)
  (get-destination inputnr (amap-dest mp) (amap-in mp) (amap-r mp)))

;; get-maps: list-of-maps
(define (get-maps lines)
  (local ((define (get-maps lines current-map result)
            (cond
              [(empty? lines) (append result (list current-map))];; place new map in back
              [(string=? (first lines) "") (get-maps (rest (rest lines)) (make-amap empty empty empty) (append result (list current-map)))];; also pass header
              [else (let
                        ([linelist (string-split (first lines) " ")])
                        (get-maps (rest lines) (struct-copy amap current-map
                                                            [dest (cons (string->number (first linelist)) (amap-dest current-map))]
                                                            [in (cons (string->number (first (rest linelist))) (amap-in current-map))]
                                                            [r (cons (string->number (first (rest (rest linelist)))) (amap-r current-map))])
                                  result))])))
    (get-maps (rest lines) (make-amap empty empty empty) empty)))

(define allmaps (get-maps maps))
(define alltestmaps (get-maps testmaps))

;; Need function: map -> function
;; produce a function of the mapping.
;; Compose reverse functions?

;; Reverse map: Just switch destination and input
(define (reverse-map amap)
  (make-amap (amap-in amap) (amap-dest amap) (amap-r amap)))


(define (amap->function amap)
  (lambda (n) (applymap amap n)))


(define location->seed ;; function
  (let ([functions (map (lambda (mp) (amap->function (reverse-map mp))) (reverse allmaps))])
    (foldl compose (first functions) (rest functions))))


(define seed->location ;; function
  (let ([functions (map (lambda (mp) (amap->function mp)) allmaps)])
    (foldl compose (first functions) (rest functions))))

(define (corresponds-to-seed? loc)
  (local ((define (corresponds-to-seed? s slist)
            (cond
              [(empty? slist) false]
              [(and (>= s (first slist))
                    (<= s (+ (first slist) (first (rest slist))))) true]
              [else (corresponds-to-seed? s (rest (rest slist)))])))
    (corresponds-to-seed? (location->seed loc) seeds)))

(define (reverse-find)
  (local ((define (reverse-find c)
            (cond
              [(corresponds-to-seed? c) c]
              [else (reverse-find (+ c 1))])))
    (reverse-find 32060088)))

(define (reverse-find2)
  (local ((define (reverse-find2 c)
            (cond
              [(not (corresponds-to-seed? c)) (+ c 1)]
              [else (reverse-find2 (- c 1))])))
    (reverse-find2 40075110)))

;;; So I did this very unorthodox...
;; First, the function composition and reversion in Racket is extremely nice, giving you a neat way to obtain the
;; location->seed function without too much trouble.

;; Then, I looked at the seed numbers. Specifically, every second number indicates a range. We can find the smallest range (16030044). That
;; means we only have to check every (/ 16030044 2) location numbers whether or not the location is a seed.
;; Thus, we started with checking loc 0, then location (/ 16030044 2), then (* 2 (/ 16030044 2)) etc. until we found a location that corresponds
;; to a seed.
;; Then, we went back from the obtained location (40075110) in single steps until we found a location that does not correspond to a seed.

;; I feel dirty
