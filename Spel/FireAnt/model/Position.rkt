(define (new-position x y)
  (let ((type 'position)
        (position (cons x y)))
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
          ((up) (set-y! (- y 1)))
          ((down) (set-y! (+ y 1)))
          ((left) (set-x! (- x 1)))
          ((right) (set-x! (+ x 1))))))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-x) (apply get-x args))
            ((eq? cmd 'set-x) (apply set-x! args))
            ((eq? cmd 'get-y) (apply get-y args))
            ((eq? cmd 'set-y) (apply set-y! args))
            ((eq? cmd 'peek)(apply peek args))
            ((eq? cmd 'move)(apply move args))
            (else (error "Unknown command" cmd))))

    dispatch))
