(#%require compatibility/mlist (only racket/base
                                     directory-list
                                     path->string))

(load "model/Level.rkt")
(load "model/Player.rkt")

(define (new-game)
  (let ((canvas (make-window 1600 1000 "Fire Ant")))
    (define (start)
            ;; List all levels in level folder
      (let* ((levels (map path->string
                         (list->mlist
                          (directory-list "level"))))
            (player (new-player)))

        (display levels)
        (newline)))

    (define (dispatch cmd)
      (cond ((eq? cmd 'start) start)))

    dispatch))
