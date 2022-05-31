(define (new-character-view owner layer bitmap-pic . mask-pic)
  (let* ((direction 0) ;; Tile direction
         (bitmap (string-append bitmap-dir bitmap-pic))
         (mask (if (pair? mask-pic)
                 (list (string-append mask-dir (car mask-pic)))
                 '()))
         (tile (apply make-bitmap-tile (cons bitmap mask)))
         (moving #f)
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
        (set! moving (or (transition (tile 'set-x!) old-x x step)
                         (transition (tile 'set-y!) old-y y step)))
        (position 'set-moving! moving)))
    
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
            ((eq? cmd 'update!) (apply update! args))
            (else (error "Unkown command" cmd))))

    (init)
    dispatch))
