(load "view/Maze-View.rkt")
(load "view/Player-View.rkt")
(load "view/Egg-View.rkt")

(define (new-view player level)
  (let* ((canvas (make-window WINDOW-WIDTH WINDOW-HEIGHT "Fire Ant"))
         (floor-layer (canvas 'make-layer))
         (walls-layer (canvas 'make-layer))
         (egg-layer (canvas 'make-layer))
         (player-layer (canvas 'make-layer))
         (player-view (new-player-view player player-layer))
         (maze-view (new-maze-view (level 'get-maze) floor-layer walls-layer))
         (egg-views (map (lambda (egg) (new-egg-view egg egg-layer)) (level 'get-eggs)))
         (scorpion-views #f))

    (define (update model-obj ms)
      (let ((type (model-obj 'get-type)))
        (case type
          ((player) (player-view 'draw ms))
          (else (error "Unknown type" type)))))

    (define (is-updating? model-obj)
      (let ((type (model-obj 'get-type)))
        (case type
          ((player) (player-view 'is-moving?))
          (else (error "Unknown type" type)))))

    (define (set-level! new-level)
      ;; Clean up layers
      ;; Make new tiles
      ;; Add tiles to layers
      #f)

    (define (game-loop)
      (canvas 'set-update-callback!))

    (define (key-handler)
      (canvas 'set-key-callback!))

    (define (dispatch cmd . args)
      (cond ((eq? cmd 'update) (apply update args))
            ((eq? cmd 'is-updating?) (apply is-updating? args))
            ((eq? cmd 'set-level!) (apply set-level! args))
            ((eq? cmd 'game-loop) (apply game-loop args))
            ((eq? cmd 'key-handler) (apply key-handler args))
            (else (error "Unknown command" cmd))))

    dispatch))
