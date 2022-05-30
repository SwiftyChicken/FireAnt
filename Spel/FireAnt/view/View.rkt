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
(load "view/Player-View.rkt")
(load "view/Scorpion-View.rkt")

(define (new-view player level)
  (let* ((canvas (make-window WINDOW-WIDTH WINDOW-HEIGHT "Fire Ant"))
;================== LAYERS ===============================;
         (floor-layer (canvas 'make-layer))
         (walls-layer (canvas 'make-layer))
         (egg-layer (canvas 'make-layer))
         (player-layer (canvas 'make-layer))
         (scorpion-layer (canvas 'make-layer))
         (info-layer (canvas 'make-layer))
;================== VIEWS ================================;
         (player-view (new-player-view player player-layer))
         (maze-view (new-maze-view (level 'get-maze) floor-layer walls-layer))
         (egg-views (map (lambda (egg) (new-egg-view egg egg-layer)) (level 'get-eggs)))
         (scorpion-views (map (lambda (scorpion) (new-scorpion-view scorpion scorpion-layer)) (level 'get-scorpions))))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (game-loop)
      (canvas 'set-update-callback!))

    (define (key-handler)
      (canvas 'set-key-callback!))

;;;;;;;;;;;;;;;;;;; SETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (set-level! new-level)
      (set! level new-level)
      ;; Clean up layers
      (floor-layer 'empty)
      (walls-layer 'empty)
      (egg-layer 'empty)
      (player-layer 'empty)
      (scorpion-layer 'empty)
      ;; Make new tiles
      (set! player-view (new-player-view player player-layer))
      (set! maze-view (new-maze-view (level 'get-maze) floor-layer walls-layer))
      (set! egg-views (map (lambda (egg) (new-egg-view egg egg-layer)) (level 'get-eggs)))
      (set! scorpion-views (map (lambda (scorpion) (new-scorpion-view scorpion scorpion-layer)) (level 'get-scorpions))))

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
                     ((scorpion) (let* ((view (find-object object scorpion-views 
                                                           (lambda (vie to-find)
                                                             (vie 'is-owner? to-find)))))
                                   (view 'update! ms)
                                   (view 'update-color!)))
                     ((egg) (let* ((view (find-object object egg-views 
                                                           (lambda (vie to-find)
                                                             (vie 'is-owner? to-find)))))
                              (if (object 'is-taken?)
                                (view 'remove!))))
                     (else (error "Unknown type" type))))
                 (iter (cdr lst))))))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'game-loop) (apply game-loop args))
            ((eq? cmd 'key-handler) (apply key-handler args))
            ((eq? cmd 'set-level!) (apply set-level! args))
            ((eq? cmd 'update!) (apply update! args))
            (else (error "Unknown command" cmd))))

    dispatch))
