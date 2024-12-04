#!/usr/bin/racket
#lang racket

(define lines (file->lines "input"))
(define testlines (file->lines "test_input"))

(define testinstructions (string-split (first testlines) ","))
(define instructions (string-split (first lines) ","))

(define (HASH astring)
  (local ((define (HASH alist res)
            (cond
              [(empty? alist) res]
              [else (HASH (rest alist) (modulo (* 17 (+ res (first alist)))
                                               256))])))
    (HASH (map char->integer (string->list astring)) 0)))

(define testanswer1 (foldl + 0 (map HASH testinstructions)))
(define answer1 (foldl + 0 (map HASH instructions)))


;; Part two
;; think it is easiest to make each box a hash where each label refers to a position?
(define (process instruction boxes)
  (local ((define (rem label alist) ;; Returns a LIST of PAIRS
            (cond
              [(empty? alist) empty]
              [(string=? label (first (first alist)))
               (rest alist)]
              [else (append (list (first alist)) (rem label (rest alist)))]))
          (define (add label alist focal-length) ;; returns a LIST of PAIRS
            (cond
              [(empty? alist) (list (list label focal-length))]
              [(string=? (first (first alist)) label)
               (append (list (list label focal-length))  (rest alist))]
              [else (append (list (first alist)) (add label (rest alist) focal-length))]))
          (define (parse instruction)
            (let* ([spl (string-split instruction #rx"[-|=]")]
                   [label (first spl)])
                   ;[instr (first (rest spl))])
              (cond
                [(= (length spl) 1) (list label "-" 0)]
                [else (list label "=" (string->number (first (rest spl))))])))
                
          (define (parse-instruction instruction boxes)
            (let* ([res (parse instruction)]
                   [operation (first (rest res))]
                   [label (first res)]
                   [boxnr (HASH label)]
                   [fl (first (rest (rest res)))])
              (cond
                [(string=? operation "=")
                 (hash-set boxes boxnr (add label (hash-ref boxes boxnr empty)
                                            fl))]
                [(string=? operation "-")
                 (hash-set boxes boxnr (rem label (hash-ref boxes boxnr empty)))]))))
    (parse-instruction instruction boxes)))

(define (fill-boxes instr)
  (local ((define (fill-boxes instr result)
            (cond
              [(empty? instr) result]
              [(fill-boxes (rest instr) (process (first instr) result))])))
    (fill-boxes instr (make-immutable-hash))))


(define (total-focusing-power boxes)
  (local ((define (total-focusing-power boxnr alist c)
            (cond
              [(empty? alist) 0]
              [else (+ (* (+ boxnr 1) c (first (rest (first alist))))
                       (total-focusing-power boxnr (rest alist) (+ c 1)))]))
          (define (focus-power boxes keys)
            (cond
              [(empty? keys) 0]
              [else (+ (total-focusing-power (first keys) (hash-ref boxes (first keys)) 1)
                       (focus-power boxes (rest keys)))])))
    (focus-power boxes (hash-keys boxes))))

(define testanswer2 (total-focusing-power (fill-boxes testinstructions)))
(define answer2 (total-focusing-power (fill-boxes instructions)))

(define (nth alist n)
  (cond
    [(= 0 n) (first alist)]
    [else (nth (rest alist) (- n 1))]))
