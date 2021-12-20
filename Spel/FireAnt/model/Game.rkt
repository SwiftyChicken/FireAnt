(define (new-game)
  (let ((canvas (make-window 1600 1000 "Fire Ant")))
    (define (start)
     ((canvas 'set-background!) "blue"))

    (define (dispatch-game cmd)
      (cond ((eq? cmd 'start) start)))

    dispatch-game))
