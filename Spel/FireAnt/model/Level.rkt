(define (new-level player file)
  (define (get-walls file)
    0)
  (let ((maze (get-walls file))
        (player 0))
    (define (start)
      (let ((levels (directory-list "level")))
        (display levels)))

    (define (dispatch-game cmd)
      (cond ((eq? cmd 'start) start)))

    dispatch-game))
