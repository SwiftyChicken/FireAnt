(#%require compatibility/mlist (only racket/base
                                     directory-list
                                     path->string))

(load "model/Level.rkt")
(load "model/Player.rkt")

(define (new-game)
  (let ((canvas (make-window 1600 1000 "Fire Ant"))
        (level-dir "level"))

    (define (start)
      (let* ((levels (map (lambda (path) ;; List all levels in level-dir
                            (string-append level-dir
                                           "/"
                                           (path->string path)))
                          (list->mlist (directory-list level-dir))))
            (player (new-player))
            (current-level (new-level player ;; Start with level 1
                                      (car levels))))

        ((canvas 'set-update-callback!) ;; Game Loop
         (lambda (ms)
           ((current-level 'update))))))

    (define (dispatch cmd)
      (cond ((eq? cmd 'start) start)))

    dispatch))
