(define (new-maze-view owner floor-layer walls-layer)
  (let* ((height (owner 'get-height))
         (width (owner 'get-width))
         (wall-bitmap "wall.png")
         (water-bitmap "water.png")
         (floor-bitmap "floor.png"))

;;;;;;;;;;;;;;;;;;; INITIALIZATION ;;;;;;;;;;;;;;;;;;;;;;;;
    (define (init)
      (draw-floor)
      (let outer-loop ((row 0))
        (if (< row height)
          (begin (let inner-loop ((column 0))
                   (if (< column width)
                     (begin (case (owner 'get-unit row column) 
                              ((wall) (draw! row column wall-bitmap))
                              ((water) (draw! row column water-bitmap)))
                            (inner-loop (+ column 1)))))
                 (outer-loop (+ row 1))))))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-owner)
      owner)

;;;;;;;;;;;;;;;;;;; AUXILIARY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (draw-floor)
      (let ((tile (make-bitmap-tile (string-append bitmap-dir floor-bitmap))))
        ((floor-layer 'add-drawable) tile)))

    (define (draw! row column bitmap)
      (let* ((tile (make-bitmap-tile (string-append bitmap-dir bitmap)))
             (pos_x (* column TILE-SIZE))
             (pos_y (* row TILE-SIZE)))
        ((tile 'set-x!) pos_x)
        ((tile 'set-y!) pos_y)
        ((walls-layer 'add-drawable) tile)))

;;;;;;;;;;;;;;;;;;; DISPATCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (dispatch cmd . args)
      (cond ((eq? cmd 'get-owner) (apply get-owner args))
            (else (error "Unknown command" cmd))))

    (init)
    dispatch))
