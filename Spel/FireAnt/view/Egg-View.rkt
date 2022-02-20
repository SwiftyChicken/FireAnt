(define (new-egg-view owner layer)
  (let* ((bitmap (string-append bitmap-dir "egg.png"))
         (mask (string-append mask-dir "egg.png"))
         (tile (make-bitmap-tile bitmap mask))
         (removed #f))

    (define (init)
      (let ((x (* ((owner 'get-position) 'get-x)
                  TILE-SIZE))
            (y (* ((owner 'get-position) 'get-y)
                  TILE-SIZE)))
        ((tile 'set-x!) x)
        ((tile 'set-y!) y)
        ((layer 'add-drawable) tile)))

    (define (get-owner)
      owner)

    (define (get-tile)
      tile)

    (define (remove!)
      (if (not removed)
        (begin ((layer 'remove-drawable) tile)
               (set! removed #t))))

    (define (is-owner? object)
      (eq? object owner))

    (define (is-removed?)
      removed)

    (define (dispatch cmd . args)
      (cond ((eq? cmd 'draw) (apply draw args))
            ((eq? cmd 'get-owner) (apply get-owner args))
            ((eq? cmd 'get-tile) (apply get-tile args))
            ((eq? cmd 'remove!) (apply remove! args))
            ((eq? cmd 'is-owner?) (apply is-owner? args))
            ((eq? cmd 'is-removed?) (apply is-removed? args))
            (else (error "Unknown command" cmd))))

    (init)
    dispatch))
