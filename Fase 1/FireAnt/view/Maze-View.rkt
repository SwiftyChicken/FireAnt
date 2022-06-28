;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maze View ADT is verantwoordelijk voor:
;; [x] Onthoud zijn eigenaar en lagen
;; [x] Teken een muur op geschikte posities en voeg toe aan de laag
;; [x] Teken de vloer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-maze-view owner floor-layer walls-layer)
  (let* ((height (owner 'get-height))
         (width (owner 'get-width))
         (wall-bitmap (string-append bitmap-dir "wall.png"))
         (floor-bitmap (string-append bitmap-dir "floor.png")))

;;;;;;;;;;;;;;;;;;; INITIALIZATION ;;;;;;;;;;;;;;;;;;;;;;;;
    (define (init)
      (draw-floor)
      (let outer-loop ((row 0))
        (if (< row height)
          (begin (let inner-loop ((column 0))
                   (if (< column width)
                     (begin (if (owner 'is-wall? row column)
                              (draw-wall row column))
                            (inner-loop (+ column 1)))))
                 (outer-loop (+ row 1))))))

;;;;;;;;;;;;;;;;;;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (get-owner)
      owner)

;;;;;;;;;;;;;;;;;;; AUXILIARY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (draw-floor)
      (let ((tile (make-bitmap-tile floor-bitmap)))
        ((floor-layer 'add-drawable) tile)))

    (define (draw-wall row column)
      (let* ((tile (make-bitmap-tile wall-bitmap))
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
