(define (new-positie)
  (let ((pos_x 0)
        (pos_y 0))

    (define (get-positie)
      (cons x y))
    (define (set-positie x y)
      (set! pos_x x)
      (set! pos_y y))

    (define (dispatch cmd)
      (cond ((eq? cmd 'get-positie) get-positie)
            ((eq? cmd 'set-positie) set-positie)))

    dispatch))
