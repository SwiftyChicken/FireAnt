(load "view/Maze-View.rkt")
(load "view/Egg-View.rkt")
(load "view/Poison-View.rkt")
(load "view/Key-View.rkt")
(load "view/Ammo-View.rkt")
(load "view/Bullet-View.rkt")
(load "view/Health-View.rkt")
(load "view/Scoreboard-View.rkt")
(load "view/Door-View.rkt")
(load "view/Player-View.rkt")
(load "view/Scorpion-View.rkt")

(define (new-view player level scoreboard)
  (let* ((canvas (make-window WINDOW-WIDTH WINDOW-HEIGHT "Fire Ant"))
         (layers '())
         (make-layer! (lambda () ; Create layer, add it to a list and returns it
                        (let ((layer (canvas 'make-layer)))
                          (set! layers (cons layer layers))
                          layer)))
;================== LAYERS ===============================;
         (floor-layer (make-layer!))
         (walls-layer (make-layer!))
         (door-layer (make-layer!))
         (item-layer (make-layer!))
         (bullet-layer (make-layer!))
         (player-layer (make-layer!))
         (scorpion-layer (make-layer!))
         (scoreboard-layer (make-layer!))
;================== VIEWS ================================;
         (player-view #f)
         (maze-view #f)
         (scoreboard-view #f)
         (item-views '())
         (bullet-views '())
         (door-views '())
         (scorpion-views '()))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (game-loop)
      (canvas 'set-update-callback!))

    (define (key-handler)
      (canvas 'set-key-callback!))

;;;;;;;;;;;;;;;;;;; SETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (set-level! new-level)
      (set! level new-level)
      ;; Clean up layers
      (for-each (lambda (layer)
                  (layer 'empty)) 
                layers)
      ;; Make new tiles
      (init-views!))

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (update! ms)
      (if (scoreboard 'is-changed?)
        (scoreboard-view 'update!))
      (let iter ((lst (level 'get-updates)))
        (if (not (null? lst))
          (begin (let* ((object (car lst))
                        (type (object 'get-type)))
                   (case type
                     ((player) (if (object 'is-dead?)
                                 (begin (player-view 'remove!))
                                 (begin (if (player-view 'is-removed?)
                                          (player-view 'reset!)
                                          (player-view 'update! ms)))))
                     ((scorpion) (let* ((view (find-object object scorpion-views find-view)))
                                   (if (object 'is-alive?)
                                     (begin (view 'update! ms)
                                            (view 'update-color!))
                                     (view 'remove!))))
                     ((egg key health poison ammo) 
                      (let ((view (find-object object item-views find-view)))
                        (if (object 'is-taken?)
                          (view 'remove!))))
                     ((bullet) (let ((view (find-object object bullet-views find-view)))
                                 (if (object 'has-collided?)
                                   (if view 
                                     (begin (view 'remove!)
                                            (set! bullet-views (filter (lambda (bullet-view) (not (eq? view bullet-view))) bullet-views))))
                                   (begin (if (not view)
                                            (begin (set! view (new-bullet-view object bullet-layer))
                                                   (set! bullet-views (cons view bullet-views))))
                                          (view 'update! ms))))) 

                     ((door) (let* ((view (find-object object door-views find-view)))
                              (if (object 'is-open?)
                                (view 'remove!))))
                     (else (error "Unknown type" type))))
                 (iter (cdr lst))))))

;;;;;;;;;;;;;;;;;;; INITIALIZATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (init-views!)
      (define (make-views! new-view layer owners)
        (map (lambda (owner) (new-view owner layer)) owners))

      (define (get-new-item-view owner layer)
        (let ((new-view (case (owner 'get-type)
                          ((egg) new-egg-view)
                          ((key) new-key-view)
                          ((ammo) new-ammo-view)
                          ((health) new-health-view)
                          ((poison) new-poison-view)
                          (else (error "Unkown item type" (owner 'get-type))))))
          (new-view owner layer)))

      (set! player-view (new-player-view player player-layer))
      (set! scoreboard-view (new-scoreboard-view scoreboard scoreboard-layer))
      (set! maze-view (new-maze-view (level 'get-maze) floor-layer walls-layer))
      (set! item-views (make-views! get-new-item-view item-layer (level 'get-items)))
      (set! door-views (make-views! new-door-view door-layer (level 'get-doors)))
      (set! scorpion-views (make-views! new-scorpion-view scorpion-layer (level 'get-scorpions))))

;;;;;;;;;;;;;;;;;;; AUXILIARY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (find-view view to-find)
      (view 'is-owner? to-find))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'game-loop) (apply game-loop args))
            ((eq? cmd 'key-handler) (apply key-handler args))
            ((eq? cmd 'set-level!) (apply set-level! args))
            ((eq? cmd 'update!) (apply update! args))
            (else (error "Unknown command" cmd))))

    (init-views!)
    dispatch))
