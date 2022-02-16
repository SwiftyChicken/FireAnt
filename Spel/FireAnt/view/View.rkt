(load "view/View_Maze.rkt")

(define (new-view)
  (let ((canvas (make-window 1600 1000 "Fire Ant")))

    (define (draw object)
      #f)

    (define (dispatch cmd)
      (cond ((eq? cmd 'draw) draw)
            ((eq? cmd 'game-loop) (canvas 'set-update-callback!))))

    dispatch))
