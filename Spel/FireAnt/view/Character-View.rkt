;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player View ADT is verantwoordelijk voor:
;; [x] Maken, initialiseren en onthouden van de tile
;; [x] Onthouden van tile direction en off het verwijderd is van de laag
;; [x] Onthouden van eigenaar object en de laag waar hij in zit
;; [x] Het reseten en verwijderen van de tile
;; [x] Updaten van de tiles positie en direction
;; [x] Maken van positie verandering "animatie/transitie"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "view/Player-View.rkt")
(load "view/Scorpion-View.rkt")

(define (new-character-view owner layer)
  (let* ((character (case (owner 'get-type)
                      ((player) (new-player-view owner layer))
                      ((scorpion) (new-scorpion-view owner layer))))
         (direction 0) ;; Tile direction
         (tile (make-bitmap-tile (character 'get-bitmap) (character 'get-mask)))
         (removed #f)) ;; Removed from layer

;;;;;;;;;;;;;;;;;;; INITIALIZATION ;;;;;;;;;;;;;;;;;;;;;;;;
    (define (init) ;; Add tile to layer and give the correct start position
      (let ((x (* ((owner 'get-position) 'get-x)
                  TILE-SIZE))
            (y (* ((owner 'get-position) 'get-y)
                  TILE-SIZE)))
        ((tile 'set-x!) x)
        ((tile 'set-y!) y)
        ((layer 'add-drawable) tile)))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-owner)
      owner)

    (define (get-tile)
      tile)

;;;;;;;;;;;;;;;;;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-moving?)
      moving)

    (define (is-removed?)
      removed)

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (update! ms) ;; Update position and direction
      (update-direction!)
      (let* ((position (owner 'get-position))
             (step (* ms (position 'get-speed)))
             (old-x (tile 'get-x))
             (old-y (tile 'get-y))
             (x (* (position 'get-x)
                   TILE-SIZE))
             (y (* (position 'get-y)
                   TILE-SIZE)))
        (position 'set-moving! (or (transition (tile 'set-x!) old-x x step)
                         (transition (tile 'set-y!) old-y y step)))))
    
    (define (remove!)
      (if (not removed)
        (begin (layer 'empty)
               (set! removed #t))))

    (define (reset!)
      (set! removed #f)
      (init))

;;;;;;;;;;;;;;;;;;; AUXILIARY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (transition setter old new step) ;; Create transitioning effect for tiles going from old to new position
      (if (< (abs (- new old)) step)
          (begin (setter new)
                 #f)
          (begin (if (< new old)
                   (setter (- old step))
                   (setter (+ old step)))
                 #t)))

    (define (update-direction!)
      (define (get-new-direction)
        (case ((owner 'get-position) 'get-orientation)
          ((down) 0)
          ((left) 1)
          ((up) 2)
          ((right) 3)
          (else direction)))
      (let iter ((new-direction (get-new-direction)))
        (if (not (eq? direction new-direction))
          (begin (set! direction (modulo (+ direction 1) 4))
                 (tile 'rotate-clockwise)
                 (iter new-direction)))))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-owner) (apply get-owner args))
            ((eq? cmd 'get-tile) (apply get-tile args))
            ((eq? cmd 'is-moving?) (apply is-moving? args))
            ((eq? cmd 'is-removed?) (apply is-removed? args))
            ((eq? cmd 'remove!) (apply remove! args))
            ((eq? cmd 'reset!) (apply reset! args))
            ((eq? cmd 'update!) (apply update! args))
            (else (apply character (cons cmd args)))))

    (init)
    dispatch))
