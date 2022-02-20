(define (new-position x y)
  (let ((type 'position)
        (position (cons x y))
        (orientation #f))
;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-x)
      (car position))

    (define (get-y)
      (cdr position))

;;;;;;;;;;;;;;;;;;; SETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (set-x! new-x)
      (set-car! position new-x))

    (define (set-y! new-y)
      (set-cdr! position new-y))

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
                   (set! orientation 'right)))))

    (define (is-equal? position)
      (let ((x2 (position 'get-x))
            (y2 (position 'get-y)))
        (and (eq? x x2) (eq? y y2))))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-x) (apply get-x args))
            ((eq? cmd 'set-x) (apply set-x! args))
            ((eq? cmd 'get-y) (apply get-y args))
            ((eq? cmd 'set-y) (apply set-y! args))
            ((eq? cmd 'peek)(apply peek args))
            ((eq? cmd 'is-equal?) (apply is-equal? args))
            ((eq? cmd 'move)(apply move args))
            (else (error "Unknown command" cmd))))

    dispatch))
