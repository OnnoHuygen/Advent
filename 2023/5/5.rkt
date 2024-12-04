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

;; (define (apply-to-range mp ran)
;;   (local ((define (get-overlap-points smp ran) ;; begin point and end of overlap.
;;             (list (max (first ran) (first (rest smp)))
;;                   (min (+ (first ran) (first (rest ran)))
;;                        (+ (first (rest smp)) (first (rest (rest smp)))))))
;;           (define (apply-to-ran smp ran)
;;             (let ([overlap (get-overlap-points smp ran)]
;;                   [mindex (min (first ran) (first (rest smp)))]
;;                   [maxdex (max (+ (first ran) (first (rest ran)))
;;                                (+ (first (rest smp)) (first (rest (rest smp)))))])
;;               (cond
;;                 []

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

(define (apply-allmaps inputnr maps)
  (local ((define (apply-allmaps inputnr maplist)
            (cond
              [(empty? maplist) inputnr]
              [else (apply-allmaps (applymap (first maplist) inputnr) (rest maplist))])))
    (apply-allmaps inputnr maps)))

(define answer1 (let
                    ([locations (map (lambda (s) (apply-allmaps s allmaps)) seeds)])
                    (foldl min (first locations) (rest locations))))
(define test1 (let
                  ([locations (map (lambda (s) (apply-allmaps s alltestmaps)) testseeds)])
                  (foldl min (first locations) (rest locations))))

;; Reverse map: Just switch destination and input
(define (reverse-map amap)
  (make-amap (amap-in amap) (amap-dest amap) (amap-r amap)))


;; (define (new-seeds inseeds)
;;   (cond
;;     [(empty? inseeds) empty]
;;     [else (append (range (range (first inseeds) (+ (first inseeds) (first (rest inseeds)))))
;;                   (new-seeds (rest (rest inseeds))))]))
;; Of course, this takes way too long. billions of numbers...

;; 

;; Appproach:
;; Make composite of maps to get seednr -> location number
;; Revert map to get location->seed
;; From this map, check if any of the seeds falls into the ranges starting

;; FIRST apply m1 THEN apply m2
;; Let single map be one line in a map, so
;; '(dest input range)

(define-struct sm (dest in r))

;; Create a sorted list of single maps from a map
(define (amap->singles mp)
  (local ((define (amap->singles destinations inputs ranges)
            (cond
              [(empty? destinations) empty]
              [else (cons (make-sm (first destinations) (first inputs) (first ranges))
                          (amap->singles (rest destinations) (rest inputs) (rest ranges)))]))
          (define (qsort slist) ;; slist: list of sm
            (cond
              [(empty? slist) empty]
              [else (let ([piv (first slist)]) ;; piv is an sm
                      (append
                       (qsort (filter (lambda (s) (< (sm-dest s) (sm-dest piv))) (rest slist)))
                       (list piv)
                       (qsort (filter (lambda (s) (>= (sm-dest s) (sm-dest piv))) (rest slist)))))])))
    (let ([slist (amap->singles (amap-dest mp) (amap-in mp) (amap-r mp))])
      (qsort slist))))


;; we want the overlap between DESTINATION of first map and INPUT of second map.
;; If that is empty, the composite is just these two maps appended.
;; If it is nonzero, we need to create new mappings.
;; Edge case: if the
;; We need to do this in a sorted way, i.e. sort the destinations of the first mapping.
;; made this, see above
(define (composite smlist1 smlist2)
  (local ((define (qsort slist) ;; slist: list of sm
            (cond
              [(empty? slist) empty]
              [else (let ([piv (first slist)]) ;; piv is an sm
                      (append
                       (qsort (filter (lambda (s) (< (sm-dest s) (sm-dest piv))) (rest slist)))
                       (list piv)
                       (qsort (filter (lambda (s) (>= (sm-dest s) (sm-dest piv))) (rest slist)))))]))
          (define (construct-maps sm1 sm2 newmaps residue) ;; construct-new-sm: 
            (cond
              [(list? sm1) (print "sm1 is list")]
              [(list? sm2) (print "sm2 is list")]
              [(and (= 0 (sm-r sm1)) (= 0 (sm-r sm2))) (list newmaps residue)]
              [(> (sm-in sm2) (sm-dest sm1))
               (construct-maps (make-sm (sm-in sm2) (+ (sm-in sm1) (- (sm-in sm2) (sm-dest sm1))) (- (sm-r sm1) (- (sm-in sm2) (sm-dest sm1))))
                               sm2
                               (cons (make-sm (sm-dest sm1) (sm-in sm1) (- (sm-in sm2) (sm-dest sm1))) newmaps)
                               residue)]
              [(< (sm-in sm2) (sm-dest sm1))
               (construct-maps sm1
                               (make-sm (+ (sm-dest sm2) (- (sm-dest sm1) (sm-in sm2))) (sm-dest sm1) (- (sm-r sm2) (- (sm-dest sm1) (sm-in sm2))))
                               (cons (make-sm (sm-dest sm1) (sm-in sm1) (- (sm-in sm2) (sm-dest sm1))) newmaps)
                               residue)]
              [(>= (sm-r sm1) (sm-r sm2))
               (list (append (list (make-sm (sm-dest sm2) (sm-in sm1) (sm-r sm2))
                                   (make-sm (+ (sm-dest sm1) (sm-r sm2)) (+ (sm-in sm1) (sm-r sm2)) (- (sm-r sm1) (sm-r sm2))))
                             newmaps)
                     empty)]
              [(< (sm-r sm1) (sm-r sm2))
               (list (append (list (make-sm (sm-dest sm2) (sm-in sm1) (sm-r sm1))) newmaps)
                     (make-sm (+ (sm-dest sm2) (sm-r sm1)) (+ (sm-in sm2) (sm-r sm1)) (- (sm-r sm2) (sm-r sm1))))]))
          (define (composhite sm sorted-sm-list) ;; -> sorted-sm-list
            (cond
              [(empty? sorted-sm-list) (list sm)]
              [else
               (let ([compare-sm (first sorted-sm-list)])
                 (cond
                   [(> (sm-dest compare-sm) (+ (sm-in sm) (sm-r sm))) (append (list sm) sorted-sm-list)];; done
                   [(> (sm-in sm) (+ (sm-dest compare-sm) (sm-r compare-sm))) (cons (first sorted-sm-list) (composhite sm (rest sorted-sm-list)))] ;; no overlap
                   [(let ([result (construct-maps compare-sm sm empty empty)])
                      (cond
                        [(empty? (first (rest result))) (append (first result) (rest sorted-sm-list))]
                        [else (append (first result) (composhite (first (rest result)) (rest sorted-sm-list)))]))]))]))
          (define (comp slist1 slist2)
            (cond
              [(empty? slist1) slist2]
              [else (comp (rest slist1) (qsort (composhite (first slist1) slist2)))])))
    (comp smlist1 smlist2)))




(define (qsort slist) ;; slist: list of sm
  (cond
    [(empty? slist) empty]
    [else (let ([piv (first slist)]) ;; piv is an sm
            (append
             (qsort (filter (lambda (s) (< (sm-dest s) (sm-dest piv))) (rest slist)))
             (list piv)
             (qsort (filter (lambda (s) (>= (sm-dest s) (sm-dest piv))) (rest slist)))))]))

;; (define composite-slist (foldr composite (first (amap->singles (first alltestmaps)))
;;                                (rest (map amap->singles alltestmaps))))

(define (singles->amap slist)
  (local ((define (singles->amap slist dest in r)
            (cond
              [(empty? slist) (make-amap dest in r)]
              [else (singles->amap (rest slist)
                                   (cons (sm-dest(first slist)) dest)
                                   (cons (sm-in (first slist)) in)
                                   (cons (sm-r (first slist)) r))])))
    (singles->amap slist empty empty empty)))

(define (print-simple-maps slist)
  (cond
    [(empty? slist) void]
    [else (begin
            (println (string-append
                      (number->string (sm-dest (first slist)))
                      " "
                      (number->string (sm-in (first slist)))
                      " "
                      (number->string (sm-r (first slist)))))
            (print-simple-maps (rest slist)))]))

(define (get-sample maplist)
  (local ((define (get-sample maplist c)
            (cond
              [(>= c 100) empty]
              [else (cons (list c (apply-allmaps c maplist))
                          (get-sample maplist (+ c 1)))])))
    (get-sample maplist 0)))




;; Perhaps new idea.
;; Sort location maps by output.
;; Take the lowest output one.
;; Just need to find out:
;; To what RANGE of input numbers is this RANGE mapped.
