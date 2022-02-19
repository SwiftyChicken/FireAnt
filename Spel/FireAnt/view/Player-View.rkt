(define (new-player-view owner canvas)
  (let* ((layer (canvas 'make-layer))
         (bitmap (string-append bitmap-dir "ant.png"))
         (mask (string-append mask-dir "ant.png"))
         (tile (make-tile tile-size tile-size bitmap mask)))

    (define (init)
      ((layer 'add-drawable) tile))

    (define (get-owner)
      owner)

    (define (draw)
      (let ((x (* ((owner 'get-position) 'get-x)
                  tile-size))
            (y (* ((owner 'get-position) 'get-y)
                  tile-size)))
        ((tile 'set-x!) x)
        ((tile 'set-y!) y)))

    (define (dispatch cmd . args)
      (cond ((eq? cmd 'draw) (apply draw args))
            ((eq? cmd 'get-owner) (apply get-owner args))
            (else (error "Unknown command" cmd))))

    (init)
    dispatch))
