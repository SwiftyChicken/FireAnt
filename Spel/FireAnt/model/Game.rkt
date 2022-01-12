(#%require compatibility/mlist (only racket/base directory-list path->string))

(define (new-game)
  (let ((canvas (make-window 1600 1000 "Fire Ant")))
    (define (start)
            ;; List all levels in level folder
      (let ((levels (map path->string
                         (list->mlist
                           (directory-list "level")))))
        (display (car levels))))

    (define (dispatch-game cmd)
      (cond ((eq? cmd 'start) start)))

    dispatch-game))
