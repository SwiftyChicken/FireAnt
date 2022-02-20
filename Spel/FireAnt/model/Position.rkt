(define (new-position pos-x pos-y)
  (let ((type 'position)
        (position (cons pos-x pos-y))
        (orientation #f)
        (speed 0.17)
        (moving #f))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-x)
      (car position))

    (define (get-y)
      (cdr position))

    (define (get-speed)
      speed)

    (define (is-moving?)
      moving)

;;;;;;;;;;;;;;;;;;; SETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (set-x! new-x)
      (set-car! position new-x))

    (define (set-y! new-y)
      (set-cdr! position new-y))

    (define (set-moving! bool)
      (set! moving bool))

;;;;;;;;;;;;;;;;;;; OTHER FUNC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (peek direction)
      (let ((x (get-x))
            (y (get-y)))
        (case direction
          ((up) (set! y (- y 1)))
          ((down) (set! y (+ y 1)))
          ((left) (set! x (- x 1)))
          ((right) (set! x (+ x 1))))
        (new-position x y)))

    (define (move direction)
      (if (not moving)
        (let ((x (get-x))
              (y (get-y)))
          (case direction
            ((up) (set-y! (- y 1))
                  (set! orientation 'up))
            ((down) (set-y! (+ y 1))
                    (set! orientation 'down))
            ((left) (set-x! (- x 1))
                    (set! orientation 'left))
            ((right) (set-x! (+ x 1))
                     (set! orientation 'right)))
          (set-moving! #t))))

    (define (is-collision? position)
      (let ((x-to-compare (position 'get-x))
            (y-to-compare (position 'get-y)))
        (and (eq? (get-x) x-to-compare)
             (eq? (get-y) y-to-compare))))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-x) (apply get-x args))
            ((eq? cmd 'set-x) (apply set-x! args))
            ((eq? cmd 'get-y) (apply get-y args))
            ((eq? cmd 'set-y) (apply set-y! args))
            ((eq? cmd 'get-speed) (apply get-speed args))
            ((eq? cmd 'is-moving?) (apply is-moving? args))
            ((eq? cmd 'set-moving!) (apply set-moving! args))
            ((eq? cmd 'peek)(apply peek args))
            ((eq? cmd 'is-collision?) (apply is-collision? args))
            ((eq? cmd 'move)(apply move args))
            (else (error "Unknown command" cmd))))

    dispatch))
