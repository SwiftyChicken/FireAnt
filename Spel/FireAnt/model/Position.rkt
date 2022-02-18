(define (new-position x y)
  (let ((type 'position)
        (position (cons x y)))

    (define (get-x)
      (car position))

    (define (get-y)
      (cdr position))

    (define (set-x! new-x)
      (set-car! position new-x))

    (define (set-y! new-y)
      (set-cdr! position new-y))

    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-x) (apply get-x args))
            ((eq? cmd 'set-x) (apply set-x! args))
            ((eq? cmd 'get-y) (apply get-y args))
            ((eq? cmd 'set-y) (apply set-y! args))
            (else (error "Unknown command" cmd))))

    dispatch))
