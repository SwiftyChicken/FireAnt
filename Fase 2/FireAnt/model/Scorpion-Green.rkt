(define (new-scorpion-green position direction)
;;;;;;;;;;;;;;;;;;; DISPATCH LET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* ((race 'green))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-direction)
      direction)

    (define (get-race)
      race)
;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (update!)
      (if (not (position 'is-moving?))
        (position 'move! direction)))

    (define (turn-back!)
      (case direction
        ((up) (set! direction 'down))
        ((down) (set! direction 'up))
        ((right) (set! direction 'left))
        ((left) (set! direction 'right))))

    (define (set-direction! new-direction)
            (set! direction new-direction))
;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-direction) (apply get-direction args))
            ((eq? cmd 'get-race) (apply get-race args))
            ((eq? cmd 'turn-back!) (apply turn-back! args))
            ((eq? cmd 'set-direction!) (apply set-direction! args))
            ((eq? cmd 'update!) (apply update! args))
            (else (error "Unkown command" cmd))))
    dispatch))
