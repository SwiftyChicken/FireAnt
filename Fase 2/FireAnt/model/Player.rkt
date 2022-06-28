(define (new-player)
  (let ((type 'player)
        (position #f)
        (changed #f) ; Check if player has changed his local var.
        (lives STARTING-LIVES)
        (keys 0)
        (ammo 0)
        (boards 0)
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

    (define (get-boards)
      boards)

    (define (get-ammo)
      ammo)

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
    (define (collect! item ammount)
      (case item
        ((key) (set! keys (+ keys ammount)))
        ((ammo) (set! ammo (+ ammo ammount)))
        ((food) (set! points (+ points ammount)))
        ((health) (set! lives (+ lives ammount)))
        ((board) (set! boards (+ boards ammount)))
        (else (error "Unkown item" item)))
      (set! changed #t))

    (define (use! item)
      (collect! item -1))

    (define (reset!)
      (set! changed #t)
      (set! position #f)
      (set! lives STARTING-LIVES)
      (set! keys 0)
      (set! ammo 0)
      (set! points 0)
      (set! alive #t))

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
            ((eq? cmd 'get-boards) (apply get-boards args))
            ((eq? cmd 'get-ammo) (apply get-ammo args))
            ((eq? cmd 'get-points) (apply get-points args))
            ((eq? cmd 'set-position!) (apply set-position! args))
            ((eq? cmd 'is-dead?) (apply is-dead? args))
            ((eq? cmd 'is-changed?) (apply is-changed? args))
            ((eq? cmd 'collect!) (apply collect! args))
            ((eq? cmd 'use!) (apply use! args))
            ((eq? cmd 'reset!) (apply reset! args))
            ((eq? cmd 'die!) (apply die! args))
            ((eq? cmd 'revive!) (apply revive! args))
            ((eq? cmd 'update!) (apply update! args))
            (else (error "Unkown command" cmd))))

    dispatch))
