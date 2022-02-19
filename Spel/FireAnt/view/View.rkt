(load "view/Maze-View.rkt")
(load "view/Player-View.rkt")

(define (new-view)
  (let* ((canvas (make-window window-width window-height "Fire Ant"))
         (maze-view #f)
         (player-view #f))

    (define (update model-obj ms)
      (let ((type (model-obj 'get-type)))
        (case type
          ((maze) (if (not (and maze-view (eq? (maze-view 'get-owner)
                                               model-obj)))
                    (begin (set! maze-view (new-maze-view model-obj canvas))
                           (maze-view 'draw))))
          ((player) (if (not player-view)
                      (set! player-view (new-player-view model-obj canvas)))
                    (player-view 'draw ms))
          (else (error "Unknown type" type)))))

    (define (is-updating? model-obj)
      (let ((type (model-obj 'get-type)))
        (case type
          ((player) (player-view 'is-moving?))
          (else (error "Unknown type" type)))))

    (define (game-loop)
      (canvas 'set-update-callback!))

    (define (key-handler)
      (canvas 'set-key-callback!))

    (define (dispatch cmd . args)
      (cond ((eq? cmd 'update) (apply update args))
            ((eq? cmd 'is-updating?) (apply is-updating? args))
            ((eq? cmd 'game-loop) (apply game-loop args))
            ((eq? cmd 'key-handler) (apply key-handler args))
            (else (error "Unknown command" cmd))))

    dispatch))
