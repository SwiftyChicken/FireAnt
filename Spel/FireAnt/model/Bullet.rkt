(define (new-bullet position direction)
  (position 'set-speed! VERY-FAST-SPEED)
  (let ((type 'bullet)
        (collide #f))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-type)
      type)

    (define (get-position)
      position)

    (define (get-direction)
      direction)

;;;;;;;;;;;;;;;;;;; PREDICATE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (has-collided?)
      collide)

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (update!)
      (position 'move! direction))

    (define (collide!)
      (set! collide #t))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-type) (apply get-type args))
            ((eq? cmd 'get-position) (apply get-position args))
            ((eq? cmd 'get-direction) (apply get-direction args))
            ((eq? cmd 'has-collided?) (apply has-collided? args))
            ((eq? cmd 'collide!) (apply collide! args))
            ((eq? cmd 'update!) (apply update! args))
            (else (error "Unkown command" cmd))))

    dispatch))
