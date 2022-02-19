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
;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

    (define (get-position)
      position)

;;;;;;;;;;;;;;;;;;; SETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (set-position! pos)
      (set! position pos))

;;;;;;;;;;;;;;;;;;; OTHER FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (die!)
      (if (alive)
        (begin (set! alive #f)
               (set! lives (- lives 1)))))

    (define (is-dead)
      (not alive))

    (define (move direction)
      (let ((x (position 'get-x))
            (y (position 'get-y)))
        (case direction
          ((up) (position 'set-y (- y 1)))
          ((down) (position 'set-y (+ y 1)))
          ((left) (position 'set-x (- x 1)))
          ((right) (position 'set-x (+ x 1))))
        (display (list x y))
        (newline)))
;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'get-position) (apply get-position args))
            ((eq? cmd 'set-position) (apply set-position! args))
            ((eq? cmd 'is-dead) (apply is-dead args))
            ((eq? cmd 'die) (apply die! args))
            ((eq? cmd 'move) (apply move args))
            (else (error "Unkown command" cmd))))

    dispatch))