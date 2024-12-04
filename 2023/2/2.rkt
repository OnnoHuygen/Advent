#!/usr/bin/racket
;; lang racket
(require racket/port)
(define lines (file->lines "input"))
(define testlines (file->lines "test_input"))

(define-struct contents (r g b))
(define bag (make-contents 12 13 14))

(define-struct game (id clist));; id plus list of contents revealed.

(define (line->game line)
  (local ((define (subgame->contents gamelist r g b)
            (cond
              [(empty? gamelist) (make-contents r g b)]
              [(string=? (first (rest gamelist)) "green")
               (subgame->contents (rest (rest gamelist))
                                  r (string->number (first gamelist)) b)]
              [(string=? (first (rest gamelist)) "red")
               (subgame->contents (rest (rest gamelist))
                                  (string->number (first gamelist)) g b)]
              [(string=? (first (rest gamelist)) "blue")
               (subgame->contents (rest (rest gamelist))
                                  r g (string->number (first gamelist)))]))
          (define (line->game id games contents)
            (cond
              [(empty? games) (make-game id contents)]
              [else (line->game id (rest games)
                                (cons (subgame->contents (string-split (string-replace (first games) "," "") " ") 0 0 0)
                                      contents))])))
    (let* ([idsplit (string-split line ":")]
           [games (string-split (first (rest idsplit)) ";")])
      (line->game (string->number (first (rest (string-split (first idsplit) " "))))
                  games empty))))

(define games (map line->game lines))
(define testgames (map line->game testlines))

(define (possible? game bag)
  (local ((define (check-contents contents bag)
            (cond
              [(and (<= (contents-r contents) (contents-r bag))
                    (<= (contents-g contents) (contents-g bag))
                    (<= (contents-b contents) (contents-b bag))) true]
              [else false]))
          (define (possible? clist contents)
            (cond
              [(empty? clist) true]
              [(check-contents (first clist) bag) (possible? (rest clist) bag)]
              [else false])))
    (possible? (game-clist game) bag)))

(define answer (foldl + 0 (map game-id (filter (lambda (g) (possible? g bag)) games))))
(define testanswer (foldl + 0 (map game-id (filter (lambda (g) (possible? g bag)) testgames))))


(define (min-contents game)
  (local ((define (min-contents clist)
            (make-contents
             (foldl max 0 (map contents-r clist))
             (foldl max 0 (map contents-g clist))
             (foldl max 0 (map contents-b clist)))))
    (min-contents (game-clist game))))

(define (pow c)
  (* (contents-r c) (contents-g c) (contents-b c)))

(define answer2 (foldl + 0 (map pow (map min-contents games))))
(define testanswer2 (foldl + 0 (map pow (map min-contents testgames))))
;; (define 

;; ;; process-game: string->hash
;; ;; Put the count of every ball into a hash table
;; (define (process-game game)
;;   (local ((define 
