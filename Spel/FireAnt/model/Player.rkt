(define (new-player)
  (let ((type 'player)
        (position #f)
        (changed #f) ; Check if player has changed his local var.
        (lives STARTING-LIVES)
        (keys 0)
        (points 0)
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

    (define (get-points)
      points)

;;;;;;;;;;;;;;;;;;; SETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (set-position! pos)
      (set! position pos))

;;;;;;;;;;;;;;;;;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-dead?)
      (not alive))

    (define (is-changed?)
      changed)

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (take-key!)
      (set! changed #t)
      (set! keys (+ keys 1)))

    (define (use-key!)
      (set! changed #t)
      (set! keys (- keys 1)))

    (define (add-points! p)
      (set! changed #t)
      (set! points (+ points p)))

    (define (die!)
      (if alive
        (begin (set! alive #f)
               (set! changed #t)
               (set! lives (- lives 1)))))

    (define (revive!)
      (if (not (zero? lives))
        (begin (position 'set-orientation! 'down)
               (set! alive #t))))

    (define (update!)
      (set! changed #f))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'get-position) (apply get-position args))
            ((eq? cmd 'get-lives) (apply get-lives args))
            ((eq? cmd 'get-keys) (apply get-keys args))
            ((eq? cmd 'get-points) (apply get-points args))
            ((eq? cmd 'set-position!) (apply set-position! args))
            ((eq? cmd 'is-dead?) (apply is-dead? args))
            ((eq? cmd 'is-changed?) (apply is-changed? args))
            ((eq? cmd 'take-key!) (apply take-key! args))
            ((eq? cmd 'use-key!) (apply use-key! args))
            ((eq? cmd 'add-points!) (apply add-points! args))
            ((eq? cmd 'die!) (apply die! args))
            ((eq? cmd 'revive!) (apply revive! args))
            ((eq? cmd 'update!) (apply update! args))
            (else (error "Unkown command" cmd))))

    dispatch))
