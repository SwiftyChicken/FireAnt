(define (new-door-view owner layer)
  (let* ((bitmap (string-append bitmap-dir "door.png"))
         (tile (make-bitmap-tile bitmap))
         (removed #f))

;;;;;;;;;;;;;;;;;;; INITIALIZATION ;;;;;;;;;;;;;;;;;;;;;;;;
    (define (init)
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

;;;;;;;;;;;;;;;;;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (is-owner? object)
      (eq? object owner))

;;;;;;;;;;;;;;;;;;; DESTRUCTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (remove!)
      (if (not removed)
        (begin ((layer 'remove-drawable) tile)
               (set! removed #t))))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-owner) (apply get-owner args))
            ((eq? cmd 'is-owner?) (apply is-owner? args))
            ((eq? cmd 'remove!) (apply remove! args))
            (else (error "Unknown command" cmd))))

    (init)
    dispatch))
