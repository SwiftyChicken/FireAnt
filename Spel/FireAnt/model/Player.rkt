;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player ADT is verantwoordelijk voor:
;; [x] Heeft een type die opgevraagd kan worden
;; [x] Onthoudt zijn positie, levens en conditie (levend/doood)
;; [x] Kan sterven en terug tot leven komen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-player)
  (let ((type 'player)
        (position #f)
        (lives 3)
        (keys 0)
        (alive #t))
;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

    (define (get-position)
      position)

    (define (get-lives)
      lives)

    (define (get-keys)
      keys)

;;;;;;;;;;;;;;;;;;; SETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (set-position! pos)
      (set! position pos))

;;;;;;;;;;;;;;;;;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-dead?)
      (not alive))

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (take-key!)
      (set! keys (+ keys 1)))

    (define (use-key!)
      (set! keys (- keys 1)))

    (define (die!)
      (if alive
        (begin (set! alive #f)
               (set! lives (- lives 1)))))

    (define (revive!)
      (if (not (zero? lives))
        (begin (position 'set-orientation! 'down)
               (set! alive #t))))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'get-position) (apply get-position args))
            ((eq? cmd 'get-lives) (apply get-lives args))
            ((eq? cmd 'get-keys) (apply get-keys args))
            ((eq? cmd 'set-position!) (apply set-position! args))
            ((eq? cmd 'is-dead?) (apply is-dead? args))
            ((eq? cmd 'take-key!) (apply take-key! args))
            ((eq? cmd 'use-key!) (apply use-key! args))
            ((eq? cmd 'die!) (apply die! args))
            ((eq? cmd 'revive!) (apply revive! args))
            (else (error "Unkown command" cmd))))

    dispatch))
