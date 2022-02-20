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

    (define (is-dead?)
      (not alive))

;;;;;;;;;;;;;;;;;;; SETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (set-position! pos)
      (set! position pos))

    (define (revive!)
      (if (not (zero? lives))
        (set! alive #t)))

    (define (die!)
      (if alive
        (begin (set! alive #f)
               (set! lives (- lives 0)))))

;;;;;;;;;;;;;;;;;;; OTHER FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'get-position) (apply get-position args))
            ((eq? cmd 'set-position) (apply set-position! args))
            ((eq? cmd 'is-dead?) (apply is-dead? args))
            ((eq? cmd 'die!) (apply die! args))
            ((eq? cmd 'revive!) (apply revive! args))
            (else (error "Unkown command" cmd))))

    dispatch))
