(define (new-surf position direction)
  (position 'set-speed! FAST-SPEED)
  (let ((type 'surf)
        (arrived #f))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

    (define (get-position)
      position)

    (define (get-direction)
      direction)

;;;;;;;;;;;;;;;;;;; PREDICATE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (has-arrived?)
      arrived)

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (update!)
      (position 'move! direction))

    (define (turn-back!)
      (set! direction  (get-opposite direction)))

    (define (arrive!)
      (position 'reset-speed!)
      (set! arrived #t))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'get-position) (apply get-position args))
            ((eq? cmd 'get-direction) (apply get-direction args))
            ((eq? cmd 'has-arrived?) (apply has-arrived? args))
            ((eq? cmd 'arrive!) (apply arrive! args))
            ((eq? cmd 'turn-back!) (apply turn-back! args))
            ((eq? cmd 'update!) (apply update! args))
            (else (error "Unkown command" cmd))))
    dispatch))
