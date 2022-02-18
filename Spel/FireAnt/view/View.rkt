(load "view/View_Maze.rkt")

(define (new-view)
  (let ((canvas (make-window 1600 1000 "Fire Ant")))

    (define (draw object)
      #f)

    (define (game-loop)
      (canvas 'set-update-callback!))

    (define (dispatch cmd . args)
      (cond ((eq? cmd 'draw) (apply draw args))
            ((eq? cmd 'game-loop) (apply game-loop args))
            (else (error "Unknown command" cmd))))

    dispatch))
