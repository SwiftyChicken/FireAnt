(load "view/Maze-View.rkt")

(define (new-view)
  (let* ((canvas (make-window 1400 880 "Fire Ant"))
         (maze-view #f))

    (define (update model-obj)
      (let ((type (model-obj 'get-type)))
        (cond ((eq? type 'maze) (if (not (and maze-view (eq? (maze-view 'get-owner)
                                                             model-obj)))
                                  (set! maze-view (new-maze-view model-obj canvas)))
                                (maze-view 'draw))
              (else (error "Unknown type" type)))))

    (define (game-loop)
      (canvas 'set-update-callback!))

    (define (dispatch cmd . args)
      (cond ((eq? cmd 'update) (apply update args))
            ((eq? cmd 'game-loop) (apply game-loop args))
            (else (error "Unknown command" cmd))))

    dispatch))
