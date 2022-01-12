(#%require (only racket/base directory-list))

(define (new-game)
  (let ((canvas (make-window 1600 1000 "Fire Ant")))
    (define (start)
      (let ((levels (directory-list "level")))
        (display levels)))

    (define (dispatch-game cmd)
      (cond ((eq? cmd 'start) start)))

    dispatch-game))
