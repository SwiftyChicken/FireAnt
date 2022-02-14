(define (new-positie x y)
  (let ((pos (cons x y)))

    (define (get-positie)
      (cons x y))

    (define (set-positie x y)
      (set-car! pos x)
      (set-car! pos y))

    (define (dispatch cmd)
      (cond ((eq? cmd 'get-positie) get-positie)
            ((eq? cmd 'set-positie) set-positie)))

    dispatch))
