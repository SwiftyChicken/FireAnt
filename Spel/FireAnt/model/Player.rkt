;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player ADT is verantwoordelijk voor:
;; [x] Positie van speler onthouden
;; [x] Conditie van speler onthouden (dood/levend)
;; [x] Aantal levens van speler onthouden
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-player)
  (let ((positie #f)
        (lives 3)
        (alive #t))

    (define (set-positie! pos)
      (set! positie pos))

    (define (die!)
      (if (alive)
        (begin (set! alive #f)
               (set! lives (- lives 1)))))

    (define (dispatch cmd)
      (cond ((eq? cmd 'get-positie) positie)
            ((eq? cmd 'set-positie) set-positie!)
            ((eq? cmd 'is-dead) (not alive))
            ((eq? cmd 'die) die!))

    dispatch))
