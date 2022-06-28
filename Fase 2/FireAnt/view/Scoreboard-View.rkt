(define (new-scoreboard-view owner layer)
  (let* ((bitmap (string-append bitmap-dir "scoreboard.png"))
         (tile (make-bitmap-tile bitmap)))

;;;;;;;;;;;;;;;;;;; INITIALIZATION ;;;;;;;;;;;;;;;;;;;;;;;;
    (define (init)
      (let ((x 0)
            (y (* GRID-HEIGHT
                  TILE-SIZE)))
        ((tile 'set-x!) x)
        ((tile 'set-y!) y)
        ((layer 'add-drawable) tile))
      (update!))

    (define (update!)
      (tile 'clear)
      ((tile 'draw-text) (owner 'print-score) 20 20 5 "white")
      ((tile 'draw-text) (owner 'print-high-score) 20 20 35 "white")
      ((tile 'draw-text) (owner 'print-lives) 20 470 5 "white")
      ((tile 'draw-text) (owner 'print-level) 20 470 35 "white")
      ((tile 'draw-text) (owner 'print-keys) 20 920 5 "white")
      ((tile 'draw-text) (owner 'print-ammo) 20 920 35 "white")
      (owner 'update!))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'update!) (apply update! args))
            (else (error "Unknown command" cmd))))

    (init)
    dispatch))
