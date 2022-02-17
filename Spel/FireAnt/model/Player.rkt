;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player ADT is verantwoordelijk voor:
;; [x] Positie van speler onthouden
;; [x] Conditie van speler onthouden (dood/levend)
;; [x] Aantal levens van speler onthouden
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-player)
  (let ((type 'player)
        (position #f)
        (lives 3)
        (alive #t))

    (define (set-position! pos)
      (set! position pos))

    (define (die!)
      (if (alive)
        (begin (set! alive #f)
               (set! lives (- lives 1)))))

    (define (dispatch cmd)
      (cond ((eq? cmd 'get-type) type)
            ((eq? cmd 'get-position) position)
            ((eq? cmd 'set-position) set-position!)
            ((eq? cmd 'is-dead) (not alive))
            ((eq? cmd 'die) die!)))

    dispatch))
