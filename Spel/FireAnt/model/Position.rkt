(define (new-position x y)
  (let ((type 'position)
        (pos (cons x y)))

    (define (set-x! new-x)
      (set-car! pos new-x))

    (define (set-y! new-y)
      (set-cdr! pos new-y))

    (define (dispatch cmd)
      (cond ((eq? cmd 'get-x) (car pos))
            ((eq? cmd 'set-x) set-x!)
            ((eq? cmd 'get-y) (cdr pos))
            ((eq? cmd 'set-y) set-y!)))

    dispatch))
