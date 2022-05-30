;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View ADT is verantwoordelijk voor:
;; [x] Het onthouden van de speler en level
;; [x] Het maken van alle nodige layers
;; [x] Het maken van de nodige views
;; [x] Het updaten van de view als er objecten van modellen geüpdatet zijn
;; [x] Het aanpassen van de view als de level veranderd
;; [x] Het geven van de game loop en key handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "view/Maze-View.rkt")
(load "view/Egg-View.rkt")
(load "view/Door-View.rkt")
(load "view/Player-View.rkt")
(load "view/Scorpion-View.rkt")

(define (new-view player level)
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
         (egg-layer (make-layer!))
         (player-layer (make-layer!))
         (scorpion-layer (make-layer!))
         (info-layer (make-layer!))
;================== VIEWS ================================;
         (player-view #f)
         (maze-view #f)
         (egg-views #f)
         (door-views #f)
         (scorpion-views #f))

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
                                   (view 'update! ms)
                                   (view 'update-color!)))
                     ((egg) (let* ((view (find-object object egg-views find-view)))
                              (if (object 'is-taken?)
                                (view 'remove!))))
                     ((door) (let* ((view (find-object object door-views find-view)))
                              (if (object 'is-open?)
                                (view 'remove!))))
                     (else (error "Unknown type" type))))
                 (iter (cdr lst))))))

;;;;;;;;;;;;;;;;;;; INITIALIZATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (init-views!)
      (define (make-views! new-view layer owners)
        (map (lambda (owner) (new-view owner layer)) owners))

      (set! player-view (new-player-view player player-layer))
      (set! maze-view (new-maze-view (level 'get-maze) floor-layer walls-layer))
      (set! egg-views (make-views! new-egg-view egg-layer (level 'get-eggs)))
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
